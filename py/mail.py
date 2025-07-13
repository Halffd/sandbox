#!/bin/python
import logging
import re
import webbrowser
import aiohttp
import aiofiles
import asyncio
import os
import sys
import pyperclip
from datetime import datetime
from tempmail import EMail

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('mail.log'),
        logging.StreamHandler()
    ]
)

# Ensure the Mails directory exists in the user's home directory
mail_dir = os.path.join(os.path.expanduser('~'), 'Mails')
os.makedirs(mail_dir, exist_ok=True)
logging.info(f"Created or verified directory: {mail_dir}")

bypass = False

async def fetch_url(url):
    """Fetch the content of a URL."""
    logging.info(f"Fetching URL: {url}")
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.text()

async def process_emails(email):
    """Process incoming emails."""
    printed_mails = []
    
    while True:
        # Wait for a message
        logging.info("Waiting for new email...")
        msg = email.wait_for_message()  # Wait for up to 60 seconds
        if msg and msg.id not in printed_mails:
            printed_mails.append(msg.id)
            logging.info(f"New email received - Subject: {msg.subject}, From: {msg.from_addr}")

            # Write email details to file
            creation_date = datetime.now()
            mail_file_path = os.path.join(mail_dir, 'mail_id_emails.txt')
            async with aiofiles.open(mail_file_path, "a") as file:
                await file.write(f"Subject: {msg.subject}\n")
                await file.write(f"From: {msg.from_addr}\n")
                await file.write(f"Body: {msg.body}\n")
                await file.write(f"Creation Date: {creation_date.strftime('%Y-%m-%d %H:%M:%S')}\n")
                await file.write("---\n\n")
            logging.info(f"Email details written to file: {mail_file_path}")

            # Extract and open all <a> links
            links = re.findall(r'href=["\']?(https?://[^"\'>]+)', msg.body)
            if links:
                logging.info(f"Found {len(links)} links in the email body.")
                for link in links:
                    logging.info(f"Opening link: {link}")
                    if bypass:
                        webbrowser.open(link)  # Open the link in the default web browser
            else:
                logging.info("No links found in the email body.")

            # Check for attachments
            if msg.attachments:
                logging.info(f"Found {len(msg.attachments)} attachments.")
                for attachment in msg.attachments:
                    data = attachment.download()
                    logging.info(f"Downloaded attachment: {attachment.filename}")

                    # Print attachment data
                    try:
                        print(data.decode('utf-8'))
                    except UnicodeDecodeError:
                        logging.warning(f"Could not decode attachment {attachment.filename} as UTF-8.")

                    # Save attachment to file
                    attachment_path = os.path.join(mail_dir, attachment.filename)
                    with open(attachment_path, 'wb') as f:
                        f.write(data)
                    logging.info(f"Attachment saved as {attachment_path}")
            else:
                logging.info("No attachments found in the email.")

        await asyncio.sleep(5)

async def create(site):
    """Create a temporary email address."""
    tm = EMail()  # Use a different provider
    email = tm.address
    creation_date = datetime.now()
    logging.info(f"Created temporary email address: {email}")
    print(f"Your temporary email address is: {email}")
    pyperclip.copy(email)  # Copy email to clipboard
    logging.info("Email copied to clipboard.")

    # Write the email address to a file
    mail_file_path = os.path.join(mail_dir, f"mail_id_emails_{site.replace('.','-')}.txt")
    async with aiofiles.open(mail_file_path, "a", encoding='utf-8') as file:
        await file.write(f"Email Address: {email} - {creation_date.strftime('%Y-%m-%d %H:%M:%S')}\n")
        await file.write("-----------------------------------\n\n")
    logging.info(f"Email address written to file: {mail_file_path}")
    await process_emails(tm)

if __name__ == "__main__":
    asyncio.run(create(sys.argv[1] if len(sys.argv) > 1 else "temp"))