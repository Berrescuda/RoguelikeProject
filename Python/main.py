##########################################################################
#File name: main.py
#Author: Klemente Gilbert-Espada										
#Description: This is where the code is actually executed.
#The game is initialized, and the primary draw-input loop is executed here
##########################################################################

import curses, traceback
#kcurses is where I've been putting my custom curses functions
#levelGenerator contains the functions for creating the levels of the game
#characters contains definitions for the player and monsters
#mapObjects contains the class definitions of the map itself (tiles, levels, etc)
#display contains the functions for displaying game information 
import kcurses, levelGenerator, characters, mapObjects, display

#Just a quick and dirty tool for me to use while developing the game,
#debug gets printed in the event of a traceback.
debug = []
#Unless we exit under specific circumstances, we print a traceback.
trace = True

#The processTurn function manages the flow of the turn
#Parameters: 	The input character for this turn, the level the turn is happening on
#Returns: 		none
def processTurn(c, level, player):
	passTurn = player.takeTurn(c)

	#If a turn has passed in game time, all the characters on the level
	#get to take turns
	if passTurn:
		#This is one of the primary reasons that the level
		#keeps track of which characters are on it
		for character in level.characters:
			#The player doesn't get an automated turn
			if character.symbol != '@':
				#This will refresh every monster's line of sight,
				#and carry out various AI tasks
				character.takeTurn()

	#Refresh the player's line of sight
	player.getLineOfSight()


#Our main function initializes curses, then goes and
#defines our level size and generates our base dungeon.
#It then runs the draw-input loop that determines the flow of the game itself.
#Parameters: 	None
#Returns: 		Nothing
def main():
	# # # BASIC INITIALIZATION # # #
	#The height of our game window (In squares)
	levelHeight = 18
	#The with (ingame squares, not curses coordinates)
	levelWidth = 36

	#Initialize our screen
	mapScreen = kcurses.initCurses()
	
	# # # DUNGEON GENERATION # # # 
	#Set up an empty array that will hold the levels of our dungeon.
	dungeon = []

	#initialize our first level with the function from levelGenerator.
	#The boolean at the end of the function call 
	#tells the generator to put the player on the level.
	s = levelGenerator.generateLevel(levelHeight, levelWidth, True)
	dungeon.append(mapObjects.Level(s))
	#Generate three more levels for our dungeon.
	for i in range(4):
		s = levelGenerator.generateLevel(levelHeight, levelWidth, False)
		dungeon.append(mapObjects.Level(s))

	#ship the dungeon into the characters file
	#(weird I know, might find a better way to do this later)
	characters.updateDungeon(dungeon)

	# # # INITIALIZE PLAYER # # #
	#Find our player in the first level of the dungeon,
	#and make them easier to find in the future.
	for character in dungeon[0].characters:
		if character.symbol == '@':
			player = character
			#Initialize certain player variables
			player.name = "foobar"
			player.currentLevel = 0
	
	# # # BEGIN DRAW-INPUT LOOP # # # 
	#initialize our input character variable
	c = 0
	#while we don't recieve the quit character we keep executing the draw-getcharacter loop
	while chr(c) != 'q' and player.currentHp > 0:
		dungeon[player.currentLevel].clearTileValues()
		#draw our map and handle relevant input
		processTurn(c, dungeon[player.currentLevel], player)
		display.drawMap(mapScreen, player, dungeon[player.currentLevel].levelMap, levelHeight, levelWidth)
		#wait for a new keystroke
		c = mapScreen.getch()	

	# # # FINISH RUNNING # # # 			
	#Return the terminal to working order.
	kcurses.cursesCleanup()

try:
	main()
except:
	#If we catch an error it's important to restore the terminal
	#to it's original state or else it will be unusable.
	kcurses.cursesCleanup()
	#Provide a traceback
	if trace:
		traceback.print_exc()
	#Print a debug message if one is present
	if debug:
		print debug
