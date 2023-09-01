import pygame
import random

# Initialize Pygame
pygame.init()

# Set up display dimensions
WIDTH, HEIGHT = 640, 480
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Snake Game")

# Colors
BLACK = (0, 0, 0)
GREEN = (0, 255, 0)
RED = (255, 0, 0)

# Snake attributes
snake_pos = [100, 50]
snake_body = [[100, 50], [90, 50], [80, 50]]
snake_dir = "RIGHT"
change_to = snake_dir

# Food attributes
food_pos = [random.randrange(1, (WIDTH//10)) * 10,
            random.randrange(1, (HEIGHT//10)) * 10]
food_spawn = True

# Score
score = 0

# Game Over Flag
game_over = False

# Main game loop
while not game_over:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            game_over = True
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_UP:
                change_to = "UP"
            if event.key == pygame.K_DOWN:
                change_to = "DOWN"
            if event.key == pygame.K_LEFT:
                change_to = "LEFT"
            if event.key == pygame.K_RIGHT:
                change_to = "RIGHT"

    # Validate direction
    if change_to == "UP" and not snake_dir == "DOWN":
        snake_dir = "UP"
    if change_to == "DOWN" and not snake_dir == "UP":
        snake_dir = "DOWN"
    if change_to == "LEFT" and not snake_dir == "RIGHT":
        snake_dir = "LEFT"
    if change_to == "RIGHT" and not snake_dir == "LEFT":
        snake_dir = "RIGHT"

    # Moving the snake
    if snake_dir == "UP":
        snake_pos[1] -= 5
    if snake_dir == "DOWN":
        snake_pos[1] += 5
    if snake_dir == "LEFT":
        snake_pos[0] -= 5
    if snake_dir == "RIGHT":
        snake_pos[0] += 5

    # Snake body growing mechanism
    
    if snake_pos[0] == food_pos[0] and snake_pos[1] == food_pos[1]:
        score += 10
        food_spawn = False
        snake_body.append([-1, -1])
    print('helloooooo')
    print(snake_body)
    print(snake_pos)
    for i in range(len(snake_body)-1, 0, -1):
        snake_body[i][0] = snake_body[i-1][0]
        snake_body[i][1] = snake_body[i-1][1]
    snake_body[0][0] = snake_pos[0]
    snake_body[0][1] = snake_pos[1]
    print(snake_body)

    if not food_spawn:
        food_pos = [random.randrange(1, (WIDTH//10)) * 10,
                    random.randrange(1, (HEIGHT//10)) * 10]
        food_spawn = True

    # Draw Snake, Food, and Score
    screen.fill(BLACK)
    for pos in snake_body:
        pygame.draw.rect(screen, GREEN, pygame.Rect(pos[0], pos[1], 10, 10))

    pygame.draw.rect(screen, RED, pygame.Rect(food_pos[0], food_pos[1], 10, 10))

    # Game Over conditions
    if snake_pos[0] < 0 or snake_pos[0] > WIDTH - 10:
        game_over = True
    if snake_pos[1] < 0 or snake_pos[1] > HEIGHT - 10:
        game_over = True

    # for block in snake_body[1:]:
    #     if snake_pos[0] == block[0] and snake_pos[1] == block[1]:
    #         game_over = True

    pygame.display.update()

    pygame.time.Clock().tick(30)

pygame.quit()
