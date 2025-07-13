import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { MailListComponent } from './mail-list/mail-list.component';
import { MailDetailComponent } from './mail-detail/mail-detail.component';
import { ComposeMailComponent } from './compose-mail/compose-mail.component';

const routes: Routes = [
  { path: '', component: MailListComponent },
  { path: 'mail/:id', component: MailDetailComponent },
  { path: 'compose', component: ComposeMailComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }