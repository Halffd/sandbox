package {
    import flash.display.MovieClip;
    import flash.display.Sprite;
    import flash.events.Event;

    public class BouncingRectangle extends MovieClip {
        private var rectangle:Sprite;
        private var velocityX:Number = 5;
        private var velocityY:Number = 5;

        public function BouncingRectangle() {
            // Create the rectangle
            rectangle = new Sprite();
            rectangle.graphics.beginFill(0x00FF00); // Green color
            rectangle.graphics.drawRect(0, 0, 100, 50); // Width 100, Height 50
            rectangle.graphics.endFill();

            // Position the rectangle
            rectangle.x = 100;
            rectangle.y = 100;

            // Add the rectangle to the display list
            addChild(rectangle);

            // Add enter frame event listener for animation
            addEventListener(Event.ENTER_FRAME, onEnterFrame);
        }

        private function onEnterFrame(e:Event):void {
            // Update rectangle position
            rectangle.x += velocityX;
            rectangle.y += velocityY;

            // Check for collision with stage edges
            if (rectangle.x <= 0 || rectangle.x + rectangle.width >= stage.stageWidth) {
                velocityX *= -1; // Reverse direction on X axis
            }
            if (rectangle.y <= 0 || rectangle.y + rectangle.height >= stage.stageHeight) {
                velocityY *= -1; // Reverse direction on Y axis
            }
        }
    }
}