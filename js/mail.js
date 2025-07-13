// mail.mjs
import 'dotenv/config';
import fs from 'fs/promises';
import os from 'os';
import path from 'path';
import chalk from 'chalk';
import clipboardy from 'clipboardy';
import open from 'open';
import { JSDOM } from 'jsdom';
import winston from 'winston';
import fetch from 'node-fetch';

// Validate environment
if (!process.env.MAILSEC_API) {
  console.error('MAILSEC_API environment variable required!');
  process.exit(1);
}

// Configure logging
const logger = winston.createLogger({
  level: 'debug',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.printf(info => `${info.timestamp} - ${info.level}: ${info.message}`)
  ),
  transports: [
    new winston.transports.File({ filename: 'mail.log' }),
    new winston.transports.Console()
  ]
});

// Configuration
const MAIL_DIR = path.join(os.homedir(), 'TempMails');
const API_BASE = 'https://mailsac.com/api';
const API_KEY = process.env.MAILSEC_API;
let running = true;

// Add this near the top with other global variables
const processedMessages = new Set();

async function setupEnvironment() {
  await fs.mkdir(MAIL_DIR, { recursive: true });
  logger.info(`Using mail directory: ${MAIL_DIR}`);
}

function handleShutdown() {
  process.on('SIGINT', () => {
    logger.info('Shutting down...');
    running = false;
    process.exit(0);
  });
}

async function generateEmail() {
  const localPart = Math.random().toString(36).substring(2, 10);
  const email = `${localPart}@mailsac.com`;
  logger.info(`Generated Mailsac address: ${chalk.yellow(email)}`);
  return email;
}

async function checkInbox(email) {
  try {
      const url = `${API_BASE}/addresses/${encodeURIComponent(email)}/messages`;
      const response = await fetch(url, {
      headers: {
        'Mailsac-Key': API_KEY,
        'Accept': 'application/json'
      }
    });

    if (!response.ok) {
      throw new Error(`API Error: ${response.status} - ${await response.text()}`);
    }

    return await response.json();
  } catch (error) {
    logger.error(`Failed to check inbox: ${error.message}`);
    return [];
  }
}

async function fetchFullMessage(email, messageId) {
  try {
    const response = await fetch(
      `${API_BASE}/addresses/${encodeURIComponent(email)}/messages/${messageId}`,
      {
        headers: {
          'Mailsac-Key': API_KEY,
          'Accept': 'application/json'
        }
      }
    );

    if (!response.ok) {
      throw new Error(`Message fetch failed: ${response.status}`);
    }

    return await response.json();
  } catch (error) {
    logger.error(`Failed to fetch message: ${error.message}`);
    return null;
  }
}

async function saveEmail(email, message) {
  try {
    const sanitizedEmail = email.replace(/[^a-zA-Z0-9@]/g, '_');
    const filename = `${sanitizedEmail}_${message._id}.txt`;
    const filePath = path.join(MAIL_DIR, filename);
    
    const from = message.from?.[0] || {};
    
    // Collect links from both HTML and message.links
    let links = new Set();
    
    // Add links from message.links array if available
    if (message.links && Array.isArray(message.links)) {
      message.links.forEach(link => {
        if (link.startsWith('http')) {
          links.add({ text: 'Link from email', href: link });
        }
      });
    }

    // Add links from HTML content if available
    if (message.html) {
      const dom = new JSDOM(message.html);
      Array.from(dom.window.document.querySelectorAll('a'))
        .forEach(a => {
          if (a.href.startsWith('http')) {
            links.add({ text: a.textContent.trim(), href: a.href });
          }
        });
    }

    const linksArray = Array.from(links);

    const content = [
      `From: ${from.name || 'Unknown'} <${from.address || 'unknown'}>`,
      `Subject: ${message.subject || 'No Subject'}`,
      `Received: ${message.received || 'Unknown date'}`,
      `Body:\n${message.text || 'No text content'}`,
      '',
      linksArray.length ? 'Links found:' : 'No links found',
      ...linksArray.map(link => `- ${link.text}: ${link.href}`),
      '='.repeat(50)
    ].join('\n');

    await fs.writeFile(filePath, content);
    logger.info(`Saved email: ${filename}`);
    
    // Print the email content to stdout
    console.log(chalk.cyan('\nNew Email Received:'));
    console.log(chalk.yellow(content));
    
    // If there are links, print them in a more noticeable way
    if (linksArray.length) {
      console.log(chalk.green('\nClickable Links:'));
      linksArray.forEach(link => {
        console.log(chalk.blue(link.href));
      });

      // Open the verification link (usually the longer URL)
      const verifyLink = linksArray
        .map(l => l.href)
        .find(url => url.includes('verify') || url.includes('activate'));
      
      if (verifyLink) {
        console.log(chalk.green('\nOpening verification link...'));
        await open(verifyLink);
      }
    }
    console.log('\n');
  } catch (error) {
    logger.error(`Failed to save email: ${error.message}`);
  }
}

async function processEmail(email) {
  try {
    const messages = await checkInbox(email);
    
    if (!messages || messages.length === 0) {
      logger.debug('No messages found');
      return;
    }

    for (const msg of messages) {
      // Skip if we've already processed this message
      if (processedMessages.has(msg._id)) {
        continue;
      }

      const fullMessage = await fetchFullMessage(email, msg._id);
      if (!fullMessage) continue;

      await saveEmail(email, fullMessage);
      // Add message ID to processed set
      processedMessages.add(msg._id);

      // Process links from HTML content
      if (fullMessage.html) {
        try {
          const dom = new JSDOM(fullMessage.html);
          const links = Array.from(dom.window.document.querySelectorAll('a'))
            .map(a => a.href)
            .filter(href => href.startsWith('http'));

          for (const link of links) {
            logger.info(`Opening link: ${link}`);
            await open(link).catch(error => 
              logger.error(`Failed to open link: ${error.message}`)
            );
          }
        } catch (error) {
          logger.error(`HTML processing error: ${error.message}`);
        }
      }
    }
  } catch (error) {
    logger.error(`Email processing failed: ${error.message}`);
  }
}

async function main() {
  await setupEnvironment();
  handleShutdown();

  const email = await generateEmail();
  
  try {
    await clipboardy.write(email);
    logger.info('Email address copied to clipboard');
  } catch (error) {
    logger.warn(`Clipboard write failed: ${error.message}`);
  }

  logger.info(`Monitoring inbox for: ${chalk.green(email)}`);
  logger.info('Press Ctrl+C to exit\n');

  while (running) {
    await processEmail(email);
    await new Promise(resolve => setTimeout(resolve, 3000)); 
  }
}

main().catch(error => {
  logger.error(`Fatal error: ${error.message}`);
  process.exit(1);
});