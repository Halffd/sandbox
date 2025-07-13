    import flash.display.MovieClip;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.KeyboardEvent;
    import flash.ui.Keyboard;
    import flash.text.TextField;
    import flash.text.TextFormat;

    public class RunnerGame extends MovieClip {
        private var player:Sprite;
        private var obstacles:Array = [];
        private var powerUps:Array = [];
        private var enemies:Array = [];
        private var gravity:Number = 0.8;
        private var jumpPower:Number = -15;
        private var velocity:Number = 0;
        private var isJumping:Boolean = false;
        private var score:int = 0;
        private var scoreText:TextField;
        private var groundY:int = 300;

        public function RunnerGame() {
            // Wait for stage to be available
            addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event):void {
            removeEventListener(Event.ADDED_TO_STAGE, init);
            
            // Create ground
            var ground:Sprite = new Sprite();
            ground.graphics.beginFill(0x000000);
            ground.graphics.drawRect(0, groundY, stage.stageWidth, stage.stageHeight - groundY);
            ground.graphics.endFill();
            addChild(ground);

            // Create player
            player = new Sprite();
            player.graphics.beginFill(0x0000FF);
            player.graphics.drawRect(0, 0, 50, 50);
            player.graphics.endFill();
            player.x = 100;
            player.y = groundY - player.height; // Position on ground
            addChild(player);

            // Score text
            scoreText = new TextField();
            scoreText.defaultTextFormat = new TextFormat("Arial", 24, 0xFFFFFF);
            scoreText.text = "Score: " + score;
            scoreText.x = 10;
            scoreText.y = 10;
            addChild(scoreText);

            // Event listeners
            stage.addEventListener(Event.ENTER_FRAME, onEnterFrame);
            stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
            addEventListener(Event.ENTER_FRAME, spawnElements);
        }

        private function onEnterFrame(e:Event):void {
            // Jump physics
            if (isJumping) {
                velocity += gravity;
                player.y += velocity;
                
                // Ground collision
                if (player.y >= groundY - player.height) {
                    player.y = groundY - player.height;
                    isJumping = false;
                    velocity = 0;
                }
            }

            // Update elements
            moveElements(obstacles, 5);
            moveElements(enemies, 5);
            moveElements(powerUps, 5);

            // Check collisions
            checkCollisions(obstacles);
            checkCollisions(enemies);
            checkPowerups();
        }

        private function spawnElements(e:Event):void {
            // Spawn obstacles (1% chance per frame)
            if (Math.random() < 0.01) {
                createObstacle();
            }
            
            // Spawn enemies (0.7% chance per frame)
            if (Math.random() < 0.007) {
                createEnemy();
            }
            
            // Spawn power-ups (0.5% chance per frame)
            if (Math.random() < 0.005) {
                createPowerUp();
            }
        }

        private function createObstacle():void {
            var obstacle:Sprite = new Sprite();
            obstacle.graphics.beginFill(0xFF0000);
            obstacle.graphics.drawRect(0, 0, 40, 40);
            obstacle.graphics.endFill();
            obstacle.x = stage.stageWidth;
            obstacle.y = groundY - obstacle.height; // Place on ground
            addChild(obstacle);
            obstacles.push(obstacle);
        }

        private function createEnemy():void {
            var enemy:Sprite = new Sprite();
            enemy.graphics.beginFill(0xFFFF00);
            enemy.graphics.drawCircle(0, 0, 20);
            enemy.graphics.endFill();
            enemy.x = stage.stageWidth;
            enemy.y = groundY - enemy.height - Math.random() * 100; // Vary height
            addChild(enemy);
            enemies.push(enemy);
        }

        private function createPowerUp():void {
            var powerUp:Sprite = new Sprite();
            powerUp.graphics.beginFill(0x00FF00);
            powerUp.graphics.drawCircle(0, 0, 15);
            powerUp.graphics.endFill();
            powerUp.x = stage.stageWidth;
            powerUp.y = groundY - 100 - Math.random() * 150;
            addChild(powerUp);
            powerUps.push(powerUp);
        }

        private function moveElements(arr:Array, speed:Number):void {
            for (var i:int = arr.length - 1; i >= 0; i--) {
                var element:Sprite = arr[i];
                element.x -= speed;
                
                if (element.x < -element.width) {
                    removeChild(element);
                    arr.splice(i, 1);
                }
            }
        }

        private function checkCollisions(arr:Array):void {
            for (var i:int = arr.length - 1; i >= 0; i--) {
                var element:Sprite = arr[i];
                if (player.hitTestObject(element)) {
                    gameOver();
                    return;
                }
            }
        }

        private function checkPowerups():void {
            for (var i:int = powerUps.length - 1; i >= 0; i--) {
                var p:Sprite = powerUps[i];
                if (player.hitTestObject(p)) {
                    score += 50;
                    scoreText.text = "Score: " + score;
                    removeChild(p);
                    powerUps.splice(i, 1);
                }
            }
        }

        private function onKeyDown(e:KeyboardEvent):void {
            if (e.keyCode == Keyboard.SPACE && !isJumping) {
                isJumping = true;
                velocity = jumpPower;
            }
        }

        private function gameOver():void {
            stage.removeEventListener(Event.ENTER_FRAME, onEnterFrame);
            removeEventListener(Event.ENTER_FRAME, spawnElements);
            trace("Game Over! Final Score: " + score);
            // Add game over visual feedback here
        }
    }
}
