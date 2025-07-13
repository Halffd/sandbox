import random
import string
import smtplib
import imaplib
import email
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

class TempEmail:
    def __init__(self, smtp_server, smtp_port, imap_server, imap_port, email_domain, username, password):
        self.smtp_server = smtp_server
        self.smtp_port = smtp_port
        self.imap_server = imap_server
        self.imap_port = imap_port
        self.email_domain = email_domain
        self.username = username
        self.password = password

    def generate_temp_email(self):
        """Generate a random temporary email address."""
        random_string = ''.join(random.choices(string.ascii_lowercase + string.digits, k=10))
        return f"{random_string}@{self.email_domain}"

    def send_email(self, from_email, to_email, subject, body):
        """Send an email using SMTP."""
        msg = MIMEMultipart()
        msg['From'] = from_email
        msg['To'] = to_email
        msg['Subject'] = subject
        msg.attach(MIMEText(body, 'plain'))

        with smtplib.SMTP(self.smtp_server, self.smtp_port) as server:
            server.starttls()
            server.login(self.username, self.password)
            server.sendmail(from_email, to_email, msg.as_string())

    def fetch_emails(self, email_address):
        """Fetch emails from the temporary email address using IMAP."""
        mail = imaplib.IMAP4_SSL(self.imap_server, self.imap_port)
        mail.login(self.username, self.password)
        mail.select('inbox')

        status, messages = mail.search(None, f'(TO "{email_address}")')
        email_ids = messages[0].split()

        emails = []
        for email_id in email_ids:
            status, msg_data = mail.fetch(email_id, '(RFC822)')
            for response_part in msg_data:
                if isinstance(response_part, tuple):
                    msg = email.message_from_bytes(response_part[1])
                    emails.append({
                        'subject': msg['subject'],
                        'from': msg['from'],
                        'body': msg.get_payload(decode=True).decode()
                    })

        mail.logout()
        return emails

# Example usage
if __name__ == "__main__":
    # Replace with your email server details
    smtp_server = "smtp.example.com"
    smtp_port = 587
    imap_server = "imap.example.com"
    imap_port = 993
    email_domain = "example.com"
    username = "your_username"
    password = "your_password"

    temp_email = TempEmail(smtp_server, smtp_port, imap_server, imap_port, email_domain, username, password)

    # Generate a temporary email address
    temp_email_address = temp_email.generate_temp_email()
    print(f"Temporary Email: {temp_email_address}")

    # Send a test email
    temp_email.send_email("sender@example.com", temp_email_address, "Test Subject", "This is a test email.")

    # Fetch emails
    emails = temp_email.fetch_emails(temp_email_address)
    for email in emails:
        print(f"Subject: {email['subject']}")
        print(f"From: {email['from']}")
        print(f"Body: {email['body']}")
        print("-" * 40) 