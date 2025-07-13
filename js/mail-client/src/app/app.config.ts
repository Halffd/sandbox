import { bootstrapApplication } from '@angular/platform-browser';
import { AppComponent } from './app.component';
import { importProvidersFrom } from '@angular/core';
import { RouterModule } from '@angular/router';
import { MailListComponent } from './mail-list/mail-list.component';
import { MailDetailComponent } from './mail-detail/mail-detail.component';
import { ComposeMailComponent } from './compose-mail/compose-mail.component';
import { EmailService } from './email.service';

const routes = [
  { path: '', component: MailListComponent },
  { path: 'mail/:id', component: MailDetailComponent },
  { path: 'compose', component: ComposeMailComponent }
];

// Export the configuration if needed
export const appConfig = {
  routes
};

// Bootstrap the application
bootstrapApplication(AppComponent, {
  providers: [
    importProvidersFrom(
      RouterModule.forRoot(routes)
    ),
    EmailService
  ]
});