import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
    <div style="text-align: center; margin: 20px;">
      <h1>Mail Client</h1>
      <router-outlet></router-outlet>
    </div>
  `,
  styles: [`
    :host {
      display: block;
      background-color: #121212; /* Dark theme background */
      color: #ffffff; /* Light text color */
      height: 100vh; /* Full height */
      font-family: Arial, sans-serif; /* Font styling */
    }
  `]
})
export class AppComponent {}