import { Component } from '@angular/core';
import { CommonModule } from '@angular/common'; // Import CommonModule
import { RouterOutlet } from '@angular/router';

@Component({
  selector: 'app-root',
  imports: [CommonModule, RouterOutlet], // Include CommonModule here
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
  title = 'angularjs';
  emojis: string[] = ['😀', '🎉', '❤️', '🚀', '🌟'];
  private intervalId: any;

  ngOnInit() {
    this.startEmojiShuffle();
  }

  ngOnDestroy() {
    this.stopEmojiShuffle();
  }

  private startEmojiShuffle() {
    this.intervalId = setInterval(() => {
      this.shuffleEmojis();
    }, 1000);
  }

  private stopEmojiShuffle() {
    clearInterval(this.intervalId);
  }

  private shuffleEmojis() {
    for (let i = this.emojis.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [this.emojis[i], this.emojis[j]] = [this.emojis[j], this.emojis[i]];
    }
  }
}