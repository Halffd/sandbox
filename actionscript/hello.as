package {
    import flash.display.Sprite;
    import flash.text.TextField;
    import flash.text.TextFormat;
    import flash.events.MouseEvent;
    
    public class HelloWorld extends Sprite {
        private var textField:TextField;
        private var format:TextFormat;
        
        public function HelloWorld() {
            // Create text format
            format = new TextFormat();
            format.font = "Arial";
            format.size = 24;
            format.color = 0xFF0000; // Red color
            format.bold = true;
            
            // Create text field
            textField = new TextField();
            textField.defaultTextFormat = format;
            textField.text = "Hello World!";
            textField.width = 200;
            textField.height = 50;
            textField.x = 100;
            textField.y = 100;
            textField.selectable = false;
            
            // Add interactivity
            textField.addEventListener(MouseEvent.CLICK, onClick);
            
            // Add to display list
            addChild(textField);
        }
        
        private function onClick(e:MouseEvent):void {
            textField.text = "You clicked me!";
        }
    }
}