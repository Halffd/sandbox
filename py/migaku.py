#!/bin/python
import asyncio
from datetime import datetime
import os
import aiofiles
from tempmail import EMail
import aiohttp
import re
import pyperclip
import webbrowser
import logging
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import sys

# Create a temporary email instance
tm = EMail()

async def register(email):
    if sys.argv[1]:
        return
    try:
        #options = webdriver.ChromeOptions()
        #options.binary_location = "/usr/bin/chromium-browser"  # Path to Chromium binary

        # Initialize the browser driver
        driver = webdriver.Chrome() #(service=Service(ChromeDriverManager().install()), options=options)

        # Go to the Migaku registration page
        driver.get("https://migaku.com/signup")

        wait = WebDriverWait(driver, 20)
        email_field = wait.until(EC.presence_of_element_located((By.ID, "auth-email")))
        email_field.send_keys(email)

        password = email
        password_field = wait.until(EC.presence_of_element_located((By.ID, "auth-password")))
        password_field.send_keys(password)

        confirmSelector = '.AuthForm__checkbox > div > label'
        confirm = driver.execute_script(f"return document.querySelector('{confirmSelector}');")
        driver.execute_script("arguments[0].click();", confirm)
        #print(confirm)
        # Submit the form
        submit = driver.execute_script("return document.querySelector('div > form > button');")
        driver.execute_script("arguments[0].click();", submit)

        wait.until(EC.presence_of_element_located((By.TAG_NAME, "body")))
        time.sleep(113)

    except Exception as e:
        logging.error(f"Error in register function: {type(e).__name__}: {str(e)}")
        raise e
    finally:
        driver.quit()

async def fetch_url(url):
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.text()

async def process_emails(email):
    printed_mails = []
    
    while True:
        # Wait for a message
        msg = email.wait_for_message()  # Wait for up to 60 seconds
        if msg and msg.id not in printed_mails:
            printed_mails.append(msg.id)
            print(f"Subject: {msg.subject}, Body: {msg.body}")

            # Write email details to file
            creation_date = datetime.now()
            async with aiofiles.open("mail_id_emails.txt", "a") as file:
                await file.write(f"Subject: {msg.subject}\n")
                await file.write(f"From: {msg.from_addr}\n")
                await file.write(f"Body: {msg.body}\n")
                await file.write(f"Creation Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                await file.write("---\n\n")

            # Extract and open all <a> links
            links = re.findall(r'href=["\']?(https?://[^"\'>]+)', msg.body)
            for link in links:
                print(f"Opening link: {link}")
                webbrowser.open(link)  # Open the link in the default web browser

            # Check for attachments
            if msg.attachments:
                attachment = msg.attachments[0]
                data = attachment.download()

                # Print attachment data
                print(f"\nDownloaded attachment: {attachment.filename}")
                print(data.decode('utf-8'))

                # Save attachment to file
                with open(attachment.filename, 'wb') as f:
                    f.write(data)
                print(f"Attachment saved as {attachment.filename}")

        await asyncio.sleep(5)
async def main():
    email = tm.address
    creation_date = datetime.now()
    print(f"Your temporary email address is: {email}")
    pyperclip.copy(email)  # Copy email to clipboard
    print("Email copied to clipboard.")

    # Write the email address to a file
    async with aiofiles.open("mail_id_emails.txt", "a") as file:
        await file.write(f"Email Address: {email} - {creation_date.strftime('%Y-%m-%d %H:%M:%S')}\n")
        await file.write("-----------------------------------\n\n")

    # Run the register function
    register_task = asyncio.create_task(register(email))
    # Run the process_emails function
    process_emails_task = asyncio.create_task(process_emails(tm))  # Pass the temp email instance

    await asyncio.gather(register_task, process_emails_task)

if __name__ == "__main__":
    asyncio.run(main())
