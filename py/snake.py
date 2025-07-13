import pygame
import random
import sys
from pygame.math import Vector2

# Initialize the game
pygame.init()
pygame.display.set_caption("Python-ception: The Snaking")
cell_size = 40
cell_number = 20
screen = pygame.display.set_mode((cell_number * cell_size, cell_number * cell_size))
clock = pygame.time.Clock()

# Load assets
try:
    python_head = pygame.image.load('python_logo.png').convert_alpha()
    python_head = pygame.transform.scale(python_head, (cell_size, cell_size))
    colt_img = pygame.image.load('colt_python.png').convert_alpha()
    colt_img = pygame.transform.scale(colt_img, (cell_size, cell_size))
except:
    # Fallback if images not found
    python_head = pygame.Surface((cell_size, cell_size))
    python_head.fill((30, 72, 128))  # Python blue
    colt_img = pygame.Surface((cell_size, cell_size))
    colt_img.fill((192, 192, 192))   # Silver color for Colt Python

# Colors
PYTHON_GREEN = (53, 114, 45)
PYTHON_YELLOW = (255, 215, 0)
BACKGROUND = (20, 20, 20)
GRID_COLOR = (44, 44, 44)

class SNAKE:
    def __init__(self):
        self.body = [Vector2(5, 10), Vector2(4, 10), Vector2(3, 10)]
        self.direction = Vector2(1, 0)
        self.new_block = False
        
        # Load body images
        self.body_img = pygame.Surface((cell_size, cell_size))
        self.body_img.fill(PYTHON_GREEN)
        
    def draw_snake(self):
        for index, block in enumerate(self.body):
            x_pos = int(block.x * cell_size)
            y_pos = int(block.y * cell_size)
            block_rect = pygame.Rect(x_pos, y_pos, cell_size, cell_size)
            
            if index == 0:
                # Use Python logo for head
                screen.blit(python_head, block_rect)
            else:
                # Use green blocks for body with yellow "scales"
                pygame.draw.rect(screen, PYTHON_GREEN, block_rect)
                if index % 2:  # Add python-like scale pattern
                    pygame.draw.rect(screen, PYTHON_YELLOW, 
                                    (x_pos + 10, y_pos + 10, cell_size - 20, cell_size - 20))
    
    def move_snake(self):
        if self.new_block:
            body_copy = self.body[:]
            body_copy.insert(0, body_copy[0] + self.direction)
            self.body = body_copy[:]
            self.new_block = False
        else:
            body_copy = self.body[:-1]
            body_copy.insert(0, body_copy[0] + self.direction)
            self.body = body_copy[:]
    
    def add_block(self):
        self.new_block = True
        
    def reset(self):
        self.body = [Vector2(5, 10), Vector2(4, 10), Vector2(3, 10)]
        self.direction = Vector2(1, 0)

class FOOD:
    def __init__(self):
        self.randomize()
        
    def draw_food(self):
        food_rect = pygame.Rect(int(self.pos.x * cell_size), int(self.pos.y * cell_size), cell_size, cell_size)
        screen.blit(colt_img, food_rect)  # Colt Python revolver as the food
        
    def randomize(self):
        self.x = random.randint(0, cell_number - 1)
        self.y = random.randint(0, cell_number - 1)
        self.pos = Vector2(self.x, self.y)

class MAIN:
    def __init__(self):
        self.snake = SNAKE()
        self.food = FOOD()
        self.score = 0
        self.game_font = pygame.font.SysFont('monospace', 25)
        
    def update(self):
        self.snake.move_snake()
        self.check_collision()
        self.check_fail()
        
    def draw_elements(self):
        self.draw_grass()
        self.snake.draw_snake()
        self.food.draw_food()
        self.draw_score()
        
    def check_collision(self):
        if self.food.pos == self.snake.body[0]:
            self.food.randomize()
            self.snake.add_block()
            self.score += 1
            
            # Make sure food doesn't spawn on snake
            while self.food.pos in self.snake.body:
                self.food.randomize()
    
    def check_fail(self):
        # Check if snake hits the walls
        if not 0 <= self.snake.body[0].x < cell_number or not 0 <= self.snake.body[0].y < cell_number:
            self.game_over()
            
        # Check if snake hits itself
        for block in self.snake.body[1:]:
            if block == self.snake.body[0]:
                self.game_over()
    
    def game_over(self):
        self.snake.reset()
        self.score = 0
    
    def draw_grass(self):
        # Create a grid pattern
        for row in range(cell_number):
            for col in range(cell_number):
                if (row + col) % 2 == 0:
                    grass_rect = pygame.Rect(col * cell_size, row * cell_size, cell_size, cell_size)
                    pygame.draw.rect(screen, GRID_COLOR, grass_rect)
    
    def draw_score(self):
        score_text = f'Pythons Caught: {self.score}'
        score_surface = self.game_font.render(score_text, True, (255, 255, 255))
        score_rect = score_surface.get_rect(topleft = (10, 10))
        screen.blit(score_surface, score_rect)

# Main game loop
main_game = MAIN()
SCREEN_UPDATE = pygame.USEREVENT
pygame.time.set_timer(SCREEN_UPDATE, 150)  # Snake speed - lower is faster

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()
        if event.type == SCREEN_UPDATE:
            main_game.update()
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_UP:
                if main_game.snake.direction.y != 1:
                    main_game.snake.direction = Vector2(0, -1)
            if event.key == pygame.K_DOWN:
                if main_game.snake.direction.y != -1:
                    main_game.snake.direction = Vector2(0, 1)
            if event.key == pygame.K_LEFT:
                if main_game.snake.direction.x != 1:
                    main_game.snake.direction = Vector2(-1, 0)
            if event.key == pygame.K_RIGHT:
                if main_game.snake.direction.x != -1:
                    main_game.snake.direction = Vector2(1, 0)
    
    screen.fill(BACKGROUND)
    main_game.draw_elements()
    pygame.display.update()
    clock.tick(60)