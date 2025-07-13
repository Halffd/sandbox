import random
from collections import deque

def generate_colorful_bfs_maze(width, height):
    # Create empty maze
    maze = [['#' for _ in range(width)] for _ in range(height)]
    
    # Pick random starting point
    start_x, start_y = random.randint(0, width-1), random.randint(0, height-1)
    maze[start_y][start_x] = ' '
    
    # BFS to carve paths
    queue = deque([(start_x, start_y)])
    while queue:
        x, y = queue.popleft()
        directions = [(0,2), (2,0), (0,-2), (-2,0)]
        random.shuffle(directions)
        
        for dx, dy in directions:
            nx, ny = x + dx, y + dy
            if 0 <= nx < width and 0 <= ny < height and maze[ny][nx] == '#':
                maze[ny][nx] = ' '
                maze[y + dy//2][x + dx//2] = ' '  # Carve path between cells
                queue.append((nx, ny))
                
    return maze