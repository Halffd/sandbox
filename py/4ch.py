#!/usr/bin/env python3
"""
4chan Thread Finder - Enhanced version with wildcard support
Author: arc x
"""
import subprocess
import json
import logging
import sys
import argparse
import time
import html
import re
import pickle
import os
from datetime import datetime, timedelta
from typing import Dict, Generator, Optional, Union, List, Tuple

import requests
from fuzzywuzzy import fuzz, process
import webbrowser

# Logging setup
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Popular 4chan boards for wildcard search
POPULAR_BOARDS = [
    'g', 'v', 'a', 'b', 'pol', 'int', 'r9k', 'mu', 'fit', 'lit', 'his', 'sci',
    'vg', 'co', 'tv', 'k', 'o', 'an', 'tg', 'sp', 'asp', 'vr', 'vrpg', 'vst',
    'w', 'wg', 'i', 'ic', 'r', 's', 'soc', 'y', 'u', 'qa', 'news', 'wsr',
    'vip', 'bant', 'trash', 'aco', 'lgbt', 'mlp', 'diy', 'toy', 'x', 'adv',
    'cgl', 'ck', 'fa', 'hc', 'hm', 'jp', 'out', 'p', 'po', 'pw', 'qst',
    'wsg', 'biz', 'trv', 'f', 'n', 'c', 'e', 'gif', 'hr', 's4s', 'cm',
    'hm', 'lgbt', 'y', 'u', 'mlp', 'vp', 'vt', 'vm', 'vmg', 'vst', 'w',
    'wg', 'i', 'ic', 'r', 's', 'soc', 'y', 'u', 'qa', 'news', 'wsr', 'vip'
]

# Boards likely to have AICG threads
AICG_BOARDS = ['g', 'vg', 'b', 'pol', 'r9k', 'biz', 'sci', 'x']

def get_all_boards() -> List[str]:
    """Get list of all 4chan boards from boards.json"""
    try:
        resp = requests.get("https://a.4cdn.org/boards.json", timeout=10)
        resp.raise_for_status()
        boards_data = resp.json()
        return [board['board'] for board in boards_data['boards']]
    except Exception as e:
        logger.warning(f"Failed to fetch board list: {e}")
        return POPULAR_BOARDS

def expand_boards(board_args: List[str]) -> List[str]:
    """Expand wildcard and special board arguments"""
    expanded = []
    
    for board in board_args:
        if board == '*':
            # All boards
            expanded.extend(get_all_boards())
        elif board == 'aicg':
            # AICG-likely boards
            expanded.extend(AICG_BOARDS)
        elif board == 'popular':
            # Popular boards
            expanded.extend(POPULAR_BOARDS[:20])  # Top 20
        else:
            # Regular board name
            expanded.append(board.strip().lower())
    
    # Remove duplicates while preserving order
    seen = set()
    return [b for b in expanded if not (b in seen or seen.add(b))]

def clean_comment(comment: str) -> str:
    """Remove HTML tags and clean up comment text"""
    if not comment:
        return ""
    # Remove HTML tags
    clean = re.sub(r'<[^>]+>', '', comment)
    # Replace common HTML entities
    clean = clean.replace('&gt;', '>').replace('&lt;', '<')
    clean = clean.replace('&quot;', '"').replace('&amp;', '&')
    clean = clean.replace('<br>', ' ').replace('<br/>', ' ')
    return clean.strip()

def get_thread_value(thread: Dict, key: str, default: Optional[str] = None) -> Union[str, None]:
    """Get thread value with HTML entity decoding"""
    value = thread.get(key, default)
    if value and isinstance(value, str):
        decoded = html.unescape(value)
        if key == "com":
            return clean_comment(decoded)
        return decoded
    return value

def smart_match(text: str, pattern: str, threshold: int = 85) -> Tuple[bool, int]:
    """Enhanced fuzzy matching with multiple strategies"""
    if not text or not pattern:
        return False, 0
    
    # Clean both strings
    text_clean = clean_comment(text.lower())
    pattern_clean = pattern.lower()
    
    # Strategy 1: Exact substring match (highest priority)
    if pattern_clean in text_clean:
        return True, 100
    
    # Strategy 2: Word-based matching for better accuracy
    text_words = set(text_clean.split())
    pattern_words = set(pattern_clean.split())
    
    # If all pattern words are in text, high score
    if pattern_words.issubset(text_words):
        return True, 95
    
    # Strategy 3: Partial word matching
    word_matches = len(pattern_words.intersection(text_words))
    if word_matches > 0 and len(pattern_words) > 1:
        word_score = (word_matches / len(pattern_words)) * 100
        if word_score >= threshold:
            return True, int(word_score)
    
    # Strategy 4: Fuzzy matching (for typos)
    fuzzy_score = fuzz.partial_ratio(pattern_clean, text_clean)
    if fuzzy_score >= threshold:
        return True, fuzzy_score
    
    # Strategy 5: Token sort ratio (for reordered words)
    token_score = fuzz.token_sort_ratio(pattern_clean, text_clean)
    if token_score >= threshold:
        return True, token_score
    
    return False, max(fuzzy_score, token_score)

def gen_chan(board: str) -> Generator[Dict, None, None]:
    """Generate threads from 4chan catalog with rate limiting"""
    url = f"https://a.4cdn.org/{board}/catalog.json"
    try:
        time.sleep(0.5)  # Be nice to 4chan servers
        resp = requests.get(url, headers={"User-Agent": "Thread Finder/1.0"}, timeout=10)
        resp.raise_for_status()
        catalog = resp.json()
        
        thread_count = 0
        for page in catalog:
            for thread in page.get("threads", []):
                thread_count += 1
                yield thread
        
        logger.debug(f"Processed {thread_count} threads from /{board}/")
        
    except Exception as e:
        logger.error(f"Failed to fetch /{board}/ catalog: {e}")
        raise

def search_board(board: str, term: str, where: str, threshold: int) -> List[Tuple]:
    """Search a single board for matching threads"""
    results = []
    
    try:
        for thread in gen_chan(board):
            subject = get_thread_value(thread, "sub", "")
            comment = get_thread_value(thread, "com", "")
            thread_no = get_thread_value(thread, "no", "")

            if not thread_no:
                continue

            matched = False
            score = 0
            
            if where in ["subject", "both"] and subject:
                matched, score = smart_match(subject, term, threshold)
            if not matched and where in ["comment", "both"] and comment:
                matched, score = smart_match(comment, term, threshold)

            if matched:
                link = f"https://boards.4channel.org/{board}/thread/{thread_no}"
                results.append((link, subject, comment, score))
    
    except Exception as e:
        logger.warning(f"Error searching /{board}/: {e}")
        return []
    
    # Sort by match score (highest first)
    results.sort(key=lambda x: x[3], reverse=True)
    
    return results

def open_link(link: str):
    """Open link in browser"""
    try:
        webbrowser.open(link)
    except Exception as e:
        logger.error(f"Failed to open browser: {e}")

def main():
    parser = argparse.ArgumentParser(
        description="Find specific threads on 4chan boards",
        epilog="""
Special board arguments:
  *        - Search all boards (slow!)
  aicg     - Search AICG-likely boards (g, vg, b, pol, r9k, biz, sci, x)
  popular  - Search popular boards only
        """
    )
    parser.add_argument("board", nargs='+', help="4chan board codes or special keywords")
    parser.add_argument("search_term", help="Pattern to search for")
    parser.add_argument("--where", choices=["subject", "comment", "both"], default="both")
    parser.add_argument("--threshold", type=int, default=75, help="Matching threshold (0-100)")
    parser.add_argument("--auto", action="store_true", help="Auto open best match")
    parser.add_argument("--limit", type=int, default=20, help="Max results total")
    parser.add_argument("--board-limit", type=int, default=3, help="Max results per board")
    parser.add_argument("--fast", action="store_true", help="Stop after first match")

    args = parser.parse_args()

    # Expand wildcards and special board names
    boards = expand_boards(args.board)
    
    if not boards:
        print("No valid boards specified.")
        sys.exit(1)
    
    term = args.search_term.strip()
    where = args.where
    threshold = args.threshold

    logger.info(f"Searching {len(boards)} boards for '{term}' in {where} with threshold {threshold}")
    logger.info(f"Boards: {', '.join(boards[:10])}{' ...' if len(boards) > 10 else ''}")

    all_results = []
    boards_searched = 0
    
    for board in boards:
        boards_searched += 1
        logger.info(f"Searching /{board}/ ({boards_searched}/{len(boards)})...")
        
        try:
            results = search_board(board, term, where, threshold)
            
            # Limit results per board
            if args.board_limit:
                results = results[:args.board_limit]
            
            # Add board info to results
            for link, subject, comment, score in results:
                all_results.append((f"/{board}/", link, subject, comment, score))
                
                # Fast mode - stop after first match
                if args.fast and all_results:
                    break
            
            if args.fast and all_results:
                break
                
        except KeyboardInterrupt:
            logger.info("Search interrupted by user")
            break
        except Exception as e:
            logger.error(f"Error searching /{board}/: {e}")
            continue
    
    if not all_results:
        print(f"No threads matching '{term}' found in {where} across {boards_searched} boards.")
        sys.exit(0)

    # Sort all results by score
    all_results.sort(key=lambda x: x[4], reverse=True)
    
    # Limit total results
    if args.limit:
        all_results = all_results[:args.limit]

    print(f"\nFound {len(all_results)} matches across {boards_searched} boards:")
    print("=" * 80)
    
    for i, (board, link, subject, comment, score) in enumerate(all_results, 1):
        print(f"[{i}] {board} (Score: {score}%)")
        print(f"    {link}")
        if subject:
            print(f"    Subject: {subject}")
        if comment:
            preview = comment[:150] + "..." if len(comment) > 150 else comment
            print(f"    Comment: {preview}")
        print("-" * 80)

    if args.auto or len(all_results) == 1:
        print(f"Opening best match: {all_results[0][1]}")
        open_link(all_results[0][1])
    else:
        try:
            choice = input(f"Open a thread? (1-{len(all_results)}, or Enter to skip): ").strip()
            if choice.isdigit():
                idx = int(choice) - 1
                if 0 <= idx < len(all_results):
                    open_link(all_results[idx][1])
        except KeyboardInterrupt:
            logger.info("User canceled selection.")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        logger.info("Terminated by user")
        sys.exit(0)
    except Exception as e:
        logger.error(f"Unhandled exception: {e}")
        sys.exit(1)