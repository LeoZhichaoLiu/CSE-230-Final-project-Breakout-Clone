# CSE-230-Final-project-Burp
## Project Proposal 

### Basic Goal:
Our primary objective is to develop an engaging and strategic TUI game named "Burp" using the Brick library. The game unfolds in a confined arena where players take control of a bacterium, strategically navigating it to consume smaller bacteria while evading larger ones. The central mechanic emphasizes growth, as successful consumption of smaller bacteria leads to the expansion of the player's bacterium, with the ultimate goal of dominating the entire arena. The detailed setup and display will involve leveraging the capabilities of the Brick library to create a visually captivating arena. We plan to implement an ASCII art-inspired design, ensuring a unique and visually appealing environment that enhances the overall gaming experience. The display will not only serve as a backdrop but also convey crucial information about the bacteria entities within the arena.

### Incremental Goal:
1. Random Direction Change: Initially, the bacteria exhibits movement in four predefined directions–up, down, left and right. Each individual bacterium adheres to a fixed movement direction in the basic goal. In the Increment goal, the bacteria gains the ability to alter its movement direction randomly at any given time.
2. Active Avoidance and Attack: Adding the behaviors such that smaller bacteria tend to evade larger ones, simulating a survival instinct. Conversely, larger bacteria should actively pursue and attack smaller counterparts, creating a dynamic and strategic environment.
3. Bacteriophage(Phage): To introduce unpredictability and urgency, we plan to include the occasional appearance of phages in the arena. Phages, as powerful entities, can indiscriminately consume bacteria, posing a constant threat to players of all sizes. This event will add an element of surprise, encouraging players to adapt their strategies dynamically.


### Goal planned but may not be realized:
1. Skills: To deepen the strategic elements of "Burp," we will introduce a skill progression tied to the growth of bacteria. Upon reaching a specific size threshold, players' bacteria will gain the ability to split, offering a strategic escape mechanism from larger predators. However, this skill comes at the cost of a temporary reduction in size, adding a risk-reward element to the gameplay.
2. Larger Arena: the camera only shows part of the arena. Player’s position is fixed in the middle of the camera. As the player moves, the camera follows the player and displays a new part of the arena corresponding to the player's operation. Other bacterias are not added to the arena randomly. Instead, the distribution of bacterias is determined at the beginning of the game.
3. Game Levels: Add game levels with different difficulties.


## Group Members
1. Zhichao Liu
2. Zhongyi Wang
3. Gongxuan Liu
4. Zefang Yuan


# Milestone 2: Update

## Architecture / key components

1. Basic Design Idea: Manages the game loop and overall state; Handles player's input (up/down/left/right) and updates the game state accordingly; Controls the initialization and rendering of the microbial pool and bacteria entities; Designs the basic UI including control panel, score area, game area etc.

2. Components Design<br>
  (a) Game main loop;<br>
  (b) Input handlers;<br>
  (c) Microbial arena state managements;<br>
  (d) Objects Rendering.<br>
  (e) UI Design

4. Brick Objects Design<br>
  (a) Microbial pool widget;<br>
  (b) Bacteria widgets (for both player-controlled and NPC bacteria);<br>
  (c) Event Handlers, including player's control, NPC's movement, player's information updates.

3. Interactions Design<br>
  (a) Bacteria entity model;<br>
  (b) Arena initialization logic model;<br>
  (c) Collision detection and resolution.

4. Game Rules and requirements<br>
  (a) Level tracking for both player's and NPC's information;<br>
  (b) Collision and absorption logic;<br>
  (c) Score tracking and display.

## Challenges and Solutions

1. Brick installation and initialization: while working with the Brick library, we encounter issues related to specific functionalities or understanding its documentation, including the installation on different operating systems locally. It's necessary to refer to Brick's documentation, demo examples, and community support.
   
2. Bacteria collision detection: implementing collision detection, especially handling collisions between multiple bacteria simultaneosly, would be more challenging than we could imagine. It would be necessary to consider design several algorithm, such as spatial partitioning, or redesign the arena rules, to reduce computational overhead. Optimize the collision resolution algorithm to ensure both accuracy and efficiency.

3. Enhance the implementation of game difficulty: precisely defining the nuanced aspects of difficulty proves challenging. Factors such as the speed of active evasion and attack, the proliferation of bacteriophages, and other elements contribute to the complexity. Striking a delicate balance between difficulty levels and ensuring an optimal user experience presents a formidable challenge.

## Do you expect to meet your goals until the deadline?

It might not be expected to meet all our goals before the deadline.

## If not, how will you modify your goals?

To complete the games, we should at least complete the arena setting and game initialization to start a basic game model. For the more advanced game mechanism such as leveling up and random NPC movement would be put into a lower priority to accomplish before the deadline. Currently we are working on the general UI and try to finish the basic logic of game controlling. 
