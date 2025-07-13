import requests
from bs4 import BeautifulSoup
import time
import webbrowser
from urllib.parse import urljoin

BASE_URL = "https://temp-mail.org/en/"
SESSION = requests.Session()
SESSION.headers.update({
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
    'Accept-Language': 'en-US,en;q=0.9',
})

def get_temp_email():
    response = SESSION.get(BASE_URL)
    soup = BeautifulSoup(response.text, 'html.parser')
    email_input = soup.find('input', {'id': 'mail'})
    return email_input['value'] if email_input else None

def check_inbox():
    response = SESSION.get(BASE_URL)
    soup = BeautifulSoup(response.text, 'html.parser')
    inbox = soup.find('div', {'class': 'inbox-area maillist'})
    return inbox.find_all('li', {'class': 'hide'})

def process_email(email_element):
    email_data = {
        'sender': email_element.find('span', {'class': 'inboxSenderEmail'}).text.strip(),
        'subject': email_element.find('a', {'class': 'viewLink'}).text.strip(),
        'links': []
    }
    
    # Extract links from email body (would need to open email view)
    body_response = SESSION.get(urljoin(BASE_URL, email_element.find('a', {'class': 'viewLink'})['href']))
    body_soup = BeautifulSoup(body_response.text, 'html.parser')
    email_body = body_soup.find('div', {'class': 'email-body'})
    
    if email_body:
        for link in email_body.find_all('a', href=True):
            email_data['links'].append(link['href'])
    
    return email_data

def monitor_inbox(interval=30):
    print("Starting temp email monitor...")
    email = get_temp_email()
    print(f"Your temporary email: {email}")
    
    seen_ids = set()
    
    while True:
        try:
            emails = check_inbox()
            for email in emails:
                email_id = email.get('id')
                if email_id and email_id not in seen_ids:
                    seen_ids.add(email_id)
                    email_data = process_email(email)
                    print(f"New email from {email_data['sender']}: {email_data['subject']}")
                    
                    for link in email_data['links']:
                        print(f"Opening link: {link}")
                        webbrowser.open(link)
            
            time.sleep(interval)
            
        except Exception as e:
            print(f"Error: {str(e)}")
            time.sleep(60)

if __name__ == "__main__":
    monitor_inbox()
