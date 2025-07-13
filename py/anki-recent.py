#!/usr/bin/env python3
import requests
import json
from datetime import datetime

def anki_request(action, **params):
    return requests.post('http://localhost:8765', json={
        'action': action,
        'version': 6,
        'params': params
    }).json()

def get_recent_cards(limit=500):
    # Get card IDs added in last 7 days
    query = f"added:{30}"
    card_ids = anki_request('findCards', query=query)['result']
    
    if not card_ids:
        print("No recent cards found")
        return
    
    # Sort by ID (newer cards have higher IDs) and limit
    card_ids = sorted(card_ids, reverse=True)[:limit]
    
    # Get card info
    cards_info = anki_request('cardsInfo', cards=card_ids)['result']
    
    print(f"{'Date':<12} {'Deck':<20} {'Word/Front':<30}")
    print("-" * 62)
    
    for card in cards_info:
        # Convert timestamp to readable date
        added_date = datetime.fromtimestamp(card['cardId'] // 1000).strftime('%Y-%m-%d')
        
        # Get deck name
        deck = card['deckName']
        
        # Extract front field (usually the word)
        front = card['fields']['Word']['value'] if 'Word' in card['fields'] else 'N/A'
        # Strip HTML tags
        import re
        front = re.sub('<[^<]+?>', '', front).strip()
        
        print(f"{added_date:<12} {deck[:19]:<20} {front[:29]:<30}")

if __name__ == "__main__":
    get_recent_cards()
