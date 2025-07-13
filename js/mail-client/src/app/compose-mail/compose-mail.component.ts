import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-compose-mail',
  templateUrl: './compose-mail.component.html',
  styleUrls: ['./compose-mail.component.css']
})
export class ComposeMailComponent {
  subject: string = '';
  body: string = '';

  constructor(private router: Router) { }

  sendMail(): void {
    // Logic to send email (e.g., save it or send it via an API)
    console.log('Sending Email:', { subject: this.subject, body: this.body });
    this.router.navigate(['/']);
  }
}