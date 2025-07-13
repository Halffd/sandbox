package {
    import flash.display.MovieClip;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.KeyboardEvent;
    import flash.ui.Keyboard;
    import flash.text.TextField;
    import flash.text.TextFormat;

    public class SpaceShooterGame extends MovieClip {
        private var player:Sprite;
        private var bullets:Array = [];
        private var enemies:Array = [];
        private var score:int = 0;
        private var scoreText:TextField;

        public function SpaceShooterGame() {
            // Create player spaceship
            player = new Sprite();
            player.graphics.beginFill(0x00FF00); // Green color
            player.graphics.drawRect(-25, -15, 50, 30); // Width 50, Height 30
            player.graphics.endFill();
            player.x = stage.stageWidth / 2;
            player.y = stage.stageHeight - 50;
            addChild(player);

            // Create score text
            scoreText = new TextField();
            scoreText.defaultTextFormat = new TextFormat("Arial", 24, 0xFFFFFF);
            scoreText.text = "Score: " + score;
            scoreText.x = 10;
            scoreText.y = 10;
            addChild(scoreText);

            // Add event listeners
            stage.addEventListener(Event.ENTER_FRAME, onEnterFrame);
            stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);

            // Start spawning enemies
            addEventListener(Event.ENTER_FRAME, spawnEnemy);
        }

        private function onEnterFrame(e:Event):void {
            // Move bullets and check collisions
            for (var i:int = bullets.length - 1; i >= 0; i--) {
                var bullet:Sprite = bullets[i];
                bullet.y -= 10; // Move bullets up
                if (bullet.y < 0) {
                    removeChild(bullet);
                    bullets.splice(i, 1);
                }

                // Check for collision with enemies
                for (var j:int = enemies.length - 1; j >= 0; j--) {
                    var enemy:Sprite = enemies[j];
                    if (bullet.hitTestObject(enemy)) {
                        removeChild(bullet);
                        bullets.splice(i, 1);
                        removeChild(enemy);
                        enemies.splice(j, 1);
                        score += 10; // Increase score
                        scoreText.text = "Score: " + score;
                        break; // Exit loop once bullet hits an enemy
                    }
                }
            }

            // Move enemies down
            for (var k:int = enemies.length - 1; k >= 0; k--) {
                var enemy:Sprite = enemies[k];
                enemy.y += 5; // Move enemies down
                if (enemy.y > stage.stageHeight) {
                    // Remove enemy if it goes off screen
                    removeChild(enemy);
                    enemies.splice(k, 1);
                }
            }
        }

        private function onKeyDown(e:KeyboardEvent):void {
            // Move player left and right
            if (e.keyCode == Keyboard.LEFT && player.x > 25) {
                player.x -= 15; // Move left
            } else if (e.keyCode == Keyboard.RIGHT && player.x < stage.stageWidth - 25) {
                player.x += 15; // Move right
            } else if (e.keyCode == Keyboard.SPACE) {
                shootBullet(); // Shoot bullet
            }
        }

        private function shootBullet():void {
            var bullet:Sprite = new Sprite();
            bullet.graphics.beginFill(0xFF0000); // Red color
            bullet.graphics.drawRect(-2, -10, 4, 10); // Width 4, Height 10
            bullet.graphics.endFill();
            bullet.x = player.x;
            bullet.y = player.y - 20; // Position above the player
            addChild(bullet);
            bullets.push(bullet);
        }

        private function spawnEnemy(e:Event):void {
            if (Math.random() < 0.02) { // Adjust spawn rate
                var enemy:Sprite = new Sprite();
                enemy.graphics.beginFill(0x0000FF); // Blue color
                enemy.graphics.drawRect(-15, -15, 30, 30); // Width 30, Height 30
                enemy.graphics.endFill();
                enemy.x = Math.random() * stage.stageWidth; // Random x position
                enemy.y = -15; // Start above the screen
                addChild(enemy);
                enemies.push(enemy);
            }
        }
    }
}