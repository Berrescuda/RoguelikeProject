##########################################################################
#File name: display.py
#Author: Klemente Gilbert-Espada										
#Description: This file contains the functions for drawing the game screen
#(the map of the level, the window containing info of the players stats)
#(UI stuff)
##########################################################################

import curses, kcurses, characters

#our drawMap function is where our primary in game curses calls take place
#it takes a command character, and modifies the game accordingly (which in the future
#should probably be moved out of the draw loop) it then draws the level map it is given
#based on the position of the player
#paramaters: input character, screen to draw on, levelmap to draw, the player object
def drawMap(screen, player, levelMap, levelHeight, levelWidth):
	#clear the screen of any leftover residue
	screen.clear()

	#Draw a box around the map screen
	kcurses.drawBox(0, 0, levelHeight, levelWidth, screen)

	#draw a box around our output zone
	kcurses.drawBox(levelHeight + 1, 0, 4, levelWidth, screen)

	characters.drawLog(levelHeight + 2, 1, screen)

	#draw our stats display window:
	statsWindow(0, levelWidth + 1, levelHeight, 23, player, screen)
	
	#this is the block of code where we draw the actual map itself
	
	#We start in the center of the screen and work outwards from there.
	y = levelHeight/2
	#We go down the map row by row
	for row in levelMap:
		#We initialize X here because we start at the center of the screen,
		#but for asthetic reasons, every in game x coordinate is two spaces
		#as far as curses is concerned
		x = levelWidth/4
		#Make sure we don't collide with the edges of the map and try to draw
		#things that aren't there.
		if y - player.yPos > 0 and y - player.yPos < levelHeight:
			#Iterate over every square on the row
			for square in row:
				#Get the information about how to display that square
				squareDisplay = square.printTile()
				#Make sure we're not going to collide with either side of our window
				if x - player.xPos > 0 and (x * 2) - (player.xPos * 2) < levelWidth:
					#Print the current square to the screen
					screen.addstr(y - player.yPos, (x * 2) - (player.xPos * 2), squareDisplay[0], curses.color_pair(squareDisplay[1]))
					#print an empty space (for effect)
					screen.addstr(y - player.yPos, x - player.xPos, '')
				#Next square
				x += 1
		#next row
		y += 1
		#Refresh the screen so the changes will take effect
		screen.refresh()

#Our statsWindow function displays basic character stats in a box of the size and at
#the location of the player's choice.
#paramaters: y pos, x pos, height of window, width of window, screen to draw on
#returns nothing
def statsWindow(y, x, height, width, player, screen):
	#first we draw the window itself
	kcurses.drawBox(y, x, height, width, screen)
	x += 1
	#print the player name at the top of the window
	screen.addstr(y + 1, x, player.name)

	#player hp
	screen.addstr(y + 2, x, "hp:")

	#Figure out how long the health bar is
	healthBarLength = ((player.currentHp * 10)/player.maxHp)
	#hp bar
	kcurses.hColoredLine(y + 2, x + 3, '=', healthBarLength, curses.color_pair(curses.COLOR_GREEN), screen)
	#display current hp/max hp
	screen.addstr(y + 2, x + 14, str(player.currentHp) + "/" + str(player.maxHp))
	
	#Figure out how long the mana bar is
	manaBarLength = ((player.currentMp * 10)/player.maxMp)
	#player mp
	screen.addstr(y + 3, x, "mp:")
	#mp bar
	kcurses.hColoredLine(y + 3, x + 3, '=', manaBarLength, curses.color_pair(curses.COLOR_BLUE), screen)
	#display current mp/max mp
	screen.addstr(y + 3, x + 14, str(player.currentMp) + "/" + str(player.maxMp))
	
	#print player XP
	screen.addstr(y + 4, x, "XP:" + str(player.xp))

	#Print the current level of the dungeon the player is on
	screen.addstr(y + 5, x, "Current Level: " + str(player.currentLevel))

	#Print the items in the character's inventory.
	screen.addstr(y + 6, x, "Inventory:")
	for i in range(len(player.inventory)):
		screen.addstr(y + 7 + i, x, player.inventory[i].name)