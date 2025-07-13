import time
import requests
from bs4 import BeautifulSoup
import random
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

def setup_driver():
    """Set up a Selenium WebDriver with appropriate options."""
    chrome_options = Options()
    # Uncomment the line below if you want to run headless
    # chrome_options.add_argument("--headless")
    chrome_options.add_argument("--disable-blink-features=AutomationControlled")
    chrome_options.add_argument("--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36")
    
    driver = webdriver.Chrome(options=chrome_options)
    return driver

def get_temp_mail(driver):
    """Navigate to temp-mail.org and extract the email address and messages."""
    try:
        # Navigate to the temp mail website
        driver.get("https://temp-mail.org/en/")
        
        # Wait for the email address to be generated
        wait = WebDriverWait(driver, 30)
        email_element = wait.until(EC.presence_of_element_located((By.ID, "mail")))
        
        # Extract the email address
        email_address = email_element.get_attribute("value")
        print(f"Generated email address: {email_address}")
        
        # Function to check for new emails
        def check_for_emails():
            # Wait for the mail list to load
            try:
                mail_list = wait.until(EC.presence_of_element_located((By.CLASS_NAME, "mail-list")))
                emails = mail_list.find_elements(By.TAG_NAME, "li")
                
                if emails:
                    print(f"Found {len(emails)} emails:")
                    for email in emails:
                        subject = email.find_element(By.CLASS_NAME, "subject").text
                        sender = email.find_element(By.CLASS_NAME, "sender").text
                        print(f"From: {sender} - Subject: {subject}")
                        
                        # Click on the email to view its content
                        email.click()
                        
                        # Wait for the email content to load
                        content_element = wait.until(EC.presence_of_element_located((By.ID, "mail-content")))
                        content = content_element.get_attribute("innerHTML")
                        
                        # Parse the content with BeautifulSoup
                        soup = BeautifulSoup(content, "html.parser")
                        print(f"Content: {soup.get_text().strip()[:100]}...")
                        
                        # Go back to the inbox
                        driver.find_element(By.ID, "back-to-inbox").click()
                else:
                    print("No emails found.")
            except Exception as e:
                print(f"Error checking emails: {e}")
        
        # Main loop to continuously check for emails
        print("Waiting for emails. Press Ctrl+C to exit.")
        while True:
            check_for_emails()
            print("Checking for new emails in 10 seconds...")
            time.sleep(10)
            driver.refresh()
            
    except Exception as e:
        print(f"An error occurred: {e}")
    finally:
        # Don't close the driver to keep the page open
        pass

if __name__ == "__main__":
    driver = setup_driver()
    try:
        get_temp_mail(driver)
    except KeyboardInterrupt:
        print("\nStopping the script...")
    # Don't close the driver to keep the page open
    # driver.quit() 