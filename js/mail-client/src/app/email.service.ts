import { Injectable } from '@angular/core';

export interface Email {
  id: number;
  subject: string;
  body: string;
}

@Injectable({
  providedIn: 'root'
})
export class EmailService {
  private emails: Email[] = [
    { id: 1, subject: 'Hello World', body: 'This is the first email.' },
    { id: 2, subject: 'Angular Update', body: 'New features in Angular.' },
    { id: 3, subject: 'Meeting Reminder', body: 'Don\'t forget our meeting!' }
  ];

  getEmails() {
    return this.emails;
  }

  getEmail(id: number) {
    return this.emails.find(email => email.id === id);
  }
}