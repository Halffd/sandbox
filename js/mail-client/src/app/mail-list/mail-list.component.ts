import { Component, OnInit } from '@angular/core';
import { EmailService, Email } from '../email.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-mail-list',
  templateUrl: './mail-list.component.html',
  styleUrls: ['./mail-list.component.css']
})
export class MailListComponent implements OnInit {
  emails: Email[] = [];

  constructor(private emailService: EmailService, private router: Router) { }

  ngOnInit(): void {
    this.emails = this.emailService.getEmails();
  }

  // Ensure this method is defined
  viewEmail(id: number): void {
    this.router.navigate(['/mail', id]);
  }
}