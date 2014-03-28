import curses, traceback
import levelGenerator

# For now we'll store some global variables here
class Gb:
	windowHeight = 18
	windowWidth = 36
	debug = []
	traceback = True

# # # # # # # # # # # # # CLASS DEFINITIONS # # # # # # # # # # # # # # # #

#Character Class, for now we'll default the character class to the player.
class Character:
	#The player has a name.
	name = "Character Name"
	#Hitpoints
	maxHp = 10
	currentHp = 10
	#Magicpoints (or what have you)
	maxMp = 10
	currentMp = maxMp
	#A display symbol
	symbol = 'M'
	#A display color
	color = curses.COLOR_RED
	#positional coordinates
	xPos = 0
	yPos = 0

	power = 1
	lineOfSight = []
	inventory = []

	#Our initialization function
	#paramaters: level we're initializing on, y position on that level,
	#x position on that level, character name
	#returns nothing
	def __init__(self, level, yPos, xPos):
		#update our personal coordinates
		self.yPos = yPos
		self.xPos = xPos
		self.level = level
		level.characters.append(self)
		level.levelMap[yPos][xPos].character = self

	def attack(self, target):
		target.currentHp -= self.power
		log(self.name +" attacks " + target.name)
		if target.currentHp == 0:
			target.die(self)

	def die(self, killer):
		self.level.levelMap[self.yPos][self.xPos].character = NullCharacter()
		self.xPos = -1
		self.yPos = -1
		log(killer.name +" killed a " +self.name)
		self.level.characters.remove(self)
		if killer == player:
			killer.xp += self.xpValue
		if self == player:
			cursesCleanup()
			print "sorry you were killed by a " + killer.name
			Gb.traceback = False
			exit(1)

	def pickup(self):
		currentTile = self.level.levelMap[self.yPos][self.xPos]
		if currentTile.contents:
			self.inventory.append(currentTile.contents[0])
			del self.level.levelMap[self.yPos][self.xPos].contents[0]


	#Our Move function moves our character on the level map.
	#paramaters: direction (tuple indicating movement in the y and x directions),
	#and map of the level we're on
	#returns a brief message if movement isn't possible
	def move(self, direction):
		#if the destination we're attempting to move to is blocked, we can't move there.
		targetYPos = self.yPos + direction[0]
		targetXPos = self.xPos + direction[1]
		levelMap = self.level.levelMap
		target = levelMap[targetYPos][targetXPos]
		if target.character.name != "null":
			self.attack(target.character)
			return "attacked target"

		if target.terrain.passable:
			#if it's not blocked we move to the destination square
			#starting by clearing out the character from the tile we're currently on
			levelMap[self.yPos][self.xPos].character = NullCharacter()
			#update our personal coordinates to the coordinates of our destination.
			self.yPos += direction[0]
			self.xPos += direction[1]
			#update the destination tile to point to our character
			target.character = self
		else:
			#if the destination square *is* blocked, we return a message
			return "path blocked"

	def getLineOfSight(self):
		self.lineOfSight = []
		n = 8
		for direction in range (8):
			for q in range (0, n + 1):
				for p in range (0, q + 1):
				 	for s in range (0, q + 1):
						eps = s
						y = 0
						for x in range(1, n + 1):
							eps += p
							if (eps >= q):
								eps -= q
								if direction & 2:
									y -= 1
								else:
									y += 1
							if direction & 1:
								x = -x

							if direction &4:
								tile = self.level.levelMap[x + self.yPos][y + self.xPos]
							else: 
								tile = self.level.levelMap[y + self.yPos][x + self.xPos]
							
							self.lineOfSight.append(tile)
							if self == player:
								tile.visible = True
							if (tile.terrain.passable == False):
								break

#Our monster class is a collection of AI commands for npc's
class Monster (Character):
	path = []

	#This will be my attempted implementation of the pathfinding
	#algorithm detailed at: 
	#http://www.roguebasin.com/index.php?title=Quick_Pathfinding_in_a_Dungeon
	#Find path should take a target tile,
	#and return a list of directions to get to that tile
	def findPath(self, target):
		origin = self.level.levelMap[self.yPos][self.xPos]
		origin.pathValue = -1
		currentTile = origin
		self.level.clearTileValues()
		unexploredTiles = []
		directions = []

		while(currentTile != target):
			adjTiles = currentTile.listAdjacentTiles(self.level.levelMap)
			for tile in adjTiles:
				if tile.terrain.passable and tile != origin:
						if tile.pathValue == 0:
							if currentTile == origin:
								tile.pathValue = 1
							else:
								tile.pathValue = currentTile.pathValue + 1
							
							unexploredTiles.append(tile)
							if tile == target:
								currentTile = tile
								break
			if unexploredTiles and currentTile != target:
				currentTile = unexploredTiles.pop(0)
			
		while(1):
			adjTiles = currentTile.listAdjacentTiles(self.level.levelMap)
			for tile in adjTiles:
				if tile.pathValue == currentTile.pathValue - 1:
					if tile == origin or tile.pathValue > 0:
						y = currentTile.yPos - tile.yPos 
						x = currentTile.xPos - tile.xPos
						directions.append((y, x))
						if tile == origin:
							self.path = directions
							return 1
						currentTile = tile
						break
	def canSeeHero(self):
		playerTile = player.level.levelMap[player.yPos][player.xPos]
		if playerTile in self.lineOfSight:
			return True
		return False

	def takeTurn(self):
		self.getLineOfSight()
		if self.canSeeHero():
			self.findPath(player.level.levelMap[player.yPos][player.xPos])
		if self.path:
			self.move(self.path.pop())

class Player (Character):
	#Experience Points
	xp = 0
	#The player symbol is '@'
	symbol = '@'

class SpaceGoblin (Monster):
	name = "Space Goblin"
	symbol = 'g'
	maxHp = 5
	currentHp = maxHp
	maxMp = 0
	currentMp = 0
	xpValue = 1


#the null character is just a temporary placeholder
#for when a tile has no one in it
class NullCharacter:
	symbol = ''
	color = 0
	name = "null"


# # # # # # # # # # # # # # # # # MAP OBJECT DEFINITIONS # # # # # # # # # # # # # #
#The terrainType class is going to be the parent class
#for the different types of terrain that tiles can have
class TerrainType:
	#terrain types have a symbol that identifies them on the map
	symbol = ''
	#terrain types are either passable or not passable by the character
	passable = False
	#terrain types are displayed with a corresponding color
	color = curses.COLOR_WHITE

#Our floor class is going to be the primary type of terrain the player will travel on
class Floor (TerrainType):
	symbol = '.'
	#the player can walk on floors
	passable = True

#Our wall class is a type of terrain that will be placed next to floor tiles
class Wall (TerrainType):
	symbol = '#'
	#the player can't walk through walls
	passable = False

class UpStairs (TerrainType):
	symbol = '<'
	passable = True

class DownStairs (TerrainType):
	symbol = '>'
	passable = True

#The tile class represents a particular square on the map,
#it can be occupied by characters and contain items,
#it has a terrain type
class Tile:
	visible = False
	#tiles start empty of characters
	character = NullCharacter()
	#tiles start empty of items
	terrain = TerrainType()

	#This value will be manipulated by our pathfinding algorithm
	pathValue = 0

	#our initialization function takes a terrain type,
	#when we initialize the tile we give it that type
	def __init__(self, terrain, y, x):
		self.terrain = terrain
		self.yPos = y
		self.xPos = x
		self.contents = []

	#when we want to print our tile, generally we want to know what 
	#its symbol is and what its color is.
	#our printTile function returns those values
	#paramaters: None
	#returns: a tuple with the symbol the tile will display and the color
	#of that symbol
	def printTile(self):
		#we check to see if there's a visible character in the tile
		if self.character.symbol != '':
			#If there is a character in the tile, we return the 
			#symbol and color provided by that character
			return [self.character.symbol, self.character.color]
		elif self.contents:
			return [self.contents[0].symbol, self.contents[0].color]
			#if there's no character in the tile, we'll return the
			#symbol and color provided by the terrain
		
		return [self.terrain.symbol, self.terrain.color]

	def listAdjacentTiles(self, levelMap):
		y = self.yPos
		x = self.xPos
		adjacentTiles = []
		if y > 0:
			adjacentTiles.append(levelMap[y - 1][x])		#Up

		if y < len(levelMap) - 1:
			adjacentTiles.append(levelMap[y + 1][x])		#Down

		if x < len(levelMap[y]) - 1:
			adjacentTiles.append(levelMap[y][x + 1])		#Right

		if x > 0:
			adjacentTiles.append(levelMap[y][x - 1])		#Left

		if y < len(levelMap) - 1 and x < len(levelMap[y]) - 1:
			adjacentTiles.append(levelMap[y + 1][x + 1])	#DownRight
		
		if y > 0 and x < len(levelMap[y]) - 1:
			adjacentTiles.append(levelMap[y -1][x + 1])		#UpRight
		
		if y > 0 and x > 0:
			adjacentTiles.append(levelMap[y - 1][x - 1])	#UpLeft

		if y < len(levelMap) - 1 and x > 0:
			adjacentTiles.append(levelMap[y + 1][x - 1])	#DownLeft

		return adjacentTiles


#our level class represents a specific level of the dungeon.
#generally it has a level number, and a map of the tiles on the level.
class Level:
	#Our level map starts as an empty list
	
	#Our level number initializes at 0
	levelNumber = 0
	
	#Our intialization function takes a string that represents
	#a map of a level. Rows of the string are seperated by the /
	#character, and individual spaces are seperated by whitespace
	def __init__(self, mapString):
		self.levelMap = []
		y = 0
		self.characters = []
		characters = []

		#split our input string into rows,
		#and then iterate across them
		for row in mapString.split('/'):
			#create an empty list to append spaces to
			tileRow = []
			#iterate across each row
			x = 0
			for space in row.split():
				#identify which terrain type each character
				#signifies, and add a tile of that type.
				if space == "#":
					tileRow.append(Tile(Wall, y, x))
				elif space == ".":
					tileRow.append(Tile(Floor, y, x))

				elif space == "<":
					tileRow.append(Tile(UpStairs, y, x))
					self.UpCoordinates = [y, x]
				elif space == ">":
					tileRow.append(Tile(DownStairs, y, x))
					self.DownCoordinates = [x, y]

				elif space == "g":
					characters.append(["SpaceGoblin", self, y, x])
					tileRow.append(Tile(Floor, y, x))
				elif space == "@":
					characters.append(["Player", self, y, x])
					tileRow.append(Tile(Floor, y, x))
				elif space == "%":
					newTile = Tile(Floor, y, x)
					newTile.contents.append(Potion())
					tileRow.append(newTile)


				else:
					#if the character is unrecognized,
					#initialize an empty tile 
					#(nothing in it, basic terrain type, which is impassable)
					tileRow.append(Tile(TerrainType, y, x))
				x += 1
			#append the row to our levelMap
			self.levelMap.append(tileRow)
			y += 1
			while characters:
				newChar = characters.pop()
				if newChar[0] == "SpaceGoblin":
					SpaceGoblin(newChar[1], newChar[2], newChar[3])
				elif newChar[0] == "Player":
					Player(newChar[1], newChar[2], newChar[3])

	def clearTileValues(self):
		for row in self.levelMap:
			for tile in row:
				tile.pathValue = 0
				tile.visible = False

# # # # # # # # # # # # # # # # ITEMS # # # # # # # # # # # # # # # # # # #

class Item:
	symbol = "%"
	color = curses.COLOR_BLUE
	name = "item"
	
class Potion(Item):
	name = "potion"
	def drink(self, character):
		character.currentHp += 5
		if character.currentHp > character.maxHp:
			character.currentHp = character.maxHp
		character.inventory.remove(self)


# # # # # # # # # # # # # # GENERAL USE FUNCTIONS# # # # # # # # # # # # # #

def empty(listToEmpty):
	while listToEmpty:
		del listToEmpty[0]
def log(message):
	logRecord.append(message)

# # # # # # # # # # # # # # CURSES FUNCTIONS # # # # # # # # # # # # # # # #

#This function is necessary for cleaning up the terminal once our program
#is done running.
def cursesCleanup():
	# restore normal keyboard mode
	curses.nocbreak()
	# restore keystroke echoing
	curses.echo()
	# required cleanup call
	curses.endwin()

#This function draws a box in our curses window
#paramaters: y pos, x pos, height of box, width of box,
#screen we're drawing the box on.
#returns nothing
def drawBox(y, x, height, width, screen):
	screen.hline(y, x + 1, curses.ACS_HLINE, width -1 )
	screen.hline(height + y, x + 1, curses.ACS_HLINE, width - 1)
	screen.vline(y + 1, x, curses.ACS_VLINE, height - 1)
	screen.vline(y + 1, width + x, curses.ACS_VLINE, height - 1)
	
	screen.addch(y, x, curses.ACS_ULCORNER)
	screen.addch(height + y, x, curses.ACS_LLCORNER)
	screen.addch(y, width + x, curses.ACS_URCORNER)
	screen.addch(y + height, x + width, curses.ACS_LRCORNER)

#this function works just like curses' "hline()" function,
#except you can also designate a color for the line
#paramaters: y pos, x pos, character to draw the line with,
#number of times to draw that character, color of that character,
#screen we're drawing the line on.
#returns nothing
def hColoredLine(y, x, char, num, color, screen):
	i = 0
	while i < num:
		screen.addch(y, x + i, char, color)
		i += 1
# # # # # # # # # # # # # # IN GAME DISPLAY FUNCTIONS # # # # # # # # # # # #

#Our statsWindow function displays basic character stats in a box of the size and at
#the location of the player's choice.
#paramaters: y pos, x pos, height of window, width of window, screen to draw on
#returns nothing
def statsWindow(y, x, height, width, screen):
	#first we draw the window itself
	drawBox(y, x, height, width, screen)
	x += 1
	#print the player name at the top of the window
	screen.addstr(y + 1, x, player.name)

	#player hp
	screen.addstr(y + 2, x, "hp:")
	#hp bar
	hColoredLine(y + 2, x + 3, '=', 10, curses.color_pair(curses.COLOR_GREEN), screen)
	#display current hp/max hp
	screen.addstr(y + 2, x + 14, str(player.currentHp) + "/" + str(player.maxHp))
	
	#player mp
	screen.addstr(y + 3, x, "mp:")
	#mp bar
	hColoredLine(y + 3, x + 3, '=', 10, curses.color_pair(curses.COLOR_BLUE), screen)
	#display current mp/max mp
	screen.addstr(y + 3, x + 14, str(player.currentMp) + "/" + str(player.maxMp))
	
	#print player XP
	screen.addstr(y + 4, x, "XP:" + str(player.xp))

	screen.addstr(y + 5, x, "Current Level: " + str(player.currentLevel))

	screen.addstr(y + 6, x, "Inventory:")
	for i in range(len(player.inventory)):
		screen.addstr(y + 7 + i, x, player.inventory[i].name)

#our drawMap function is where our primary in game curses calls take place
#it takes a command character, and modifies the game accordingly (which in the future
#should probably be moved out of the draw loop) it then draws the level map it is given
#based on the position of the player
#paramaters: input character, screen to draw on, levelmap to draw, the player object
def drawMap(c, screen, player, level):
	#clear the screen of any leftover residue
	screen.clear()
	passTurn = False
	#if our command is a directional key,
	#we move our player
	if c == 65 or c == 56 or chr(c) == 'w':
		player.move([-1, 0])
		passTurn = True

	if c == 66 or c == 50 or chr(c) == 's':
		player.move([1, 0])
		passTurn = True

	if c == 67 or c == 54 or chr(c) == 'd':
		player.move([0, 1])
		passTurn = True

	if c == 68 or c == 52 or chr(c) == 'a':
		player.move([0, -1])
		passTurn = True

	if c == 49:
		player.move([+1, -1])
		passTurn = True

	if c == 51:
		player.move([+1, 1])
		passTurn = True

	if c == 55:
		player.move([-1, -1])
		passTurn = True

	if c == 57: 
		player.move([-1, 1])
		passTurn = True

	if c == 53:
		passTurn = True

	if chr(c) == 'g':
		player.pickup()

	if chr(c) == 'u':
		if player.inventory:
			player.inventory[0].drink(player)
			passTurn = True
	
	if chr(c) == '<':
		if level.levelMap[player.yPos][player.xPos].terrain.symbol == '<':
			level.characters.remove(player)
			level.levelMap[player.yPos][player.xPos].character = NullCharacter()
			
			player.currentLevel -= 1
			player.level = dungeon[player.currentLevel]
			level = player.level
			level.characters.append(player)
			
			Gb.debug = player.level.DownCoordinates
			player.yPos = level.DownCoordinates[0]
			player.xPos = level.DownCoordinates[1]
			level.levelMap[player.yPos][player.xPos].character = player
		else :
			log("Sorry, you can't go up here")
	if chr(c) == '>':
		if level.levelMap[player.yPos][player.xPos].terrain.symbol == '>':
			level.characters.remove(player)
			level.levelMap[player.yPos][player.xPos].character = NullCharacter()

			player.currentLevel += 1
			player.level = dungeon[player.currentLevel]
			level = player.level
			level.characters.append(player)
			player.yPos = level.UpCoordinates[0]
			player.xPos = level.UpCoordinates[1]
			level.levelMap[player.yPos][player.xPos].character = player
		else :
			log("Sorry, you can't go down here")

	levelMap = level.levelMap

	if passTurn:
		for character in level.characters:
			if character.symbol != '@':
				character.takeTurn()

	player.getLineOfSight()
		

	#Draw a box around the map screen
	drawBox(0, 0, Gb.windowHeight, Gb.windowWidth, screen)

	#draw a box around our output zone
	drawBox(Gb.windowHeight + 1, 0, 4, Gb.windowWidth, screen)
	if logRecord:
		screen.addstr(Gb.windowHeight +4, 1, logRecord[len(logRecord)-1])
		screen.addstr(Gb.windowHeight +3, 1, logRecord[len(logRecord) -2])
		screen.addstr(Gb.windowHeight +2, 1, logRecord[len(logRecord)-3] )

	#draw our stats display:
	statsWindow(0, Gb.windowWidth + 1, Gb.windowHeight, 23, screen)
	
	#this is the block of code where we draw the actual map itself
	y = Gb.windowHeight/2

	for row in levelMap:
		x = Gb.windowWidth/4
		if y - player.yPos > 0 and y - player.yPos < Gb.windowHeight:
			for square in row:
				squareDisplay = square.printTile()
				if square.visible and square.character.symbol == '':
					squareDisplay[1] = curses.COLOR_GREEN
				if x - player.xPos > 0 and (x * 2) - (player.xPos * 2) < Gb.windowWidth:
					screen.addstr(y - player.yPos, (x * 2) - (player.xPos * 2), squareDisplay[0], curses.color_pair(squareDisplay[1]))
					screen.addstr(y - player.yPos, x - player.xPos, '')
				x += 1
		y += 1
		screen.refresh()



 # # # # # # # # # # # # # MAIN BODY OF CODE # # # # # # # #
try:
	mapScreen = curses.initscr()
	# turn off keystroke echo
	curses.noecho()
	# hide the cursor
	curses.curs_set(0)

	# keystrokes are honored immediately, rather than waiting for the
	# user to hit Enter
	curses.cbreak()
	# start color display (if it exists; could check with has_colors())
	curses.start_color()
	# set up a foreground/background color pair that we'll use for indicating selection
	curses.init_pair(curses.COLOR_WHITE,curses.COLOR_WHITE, curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_RED,curses.COLOR_RED,curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_GREEN, curses.COLOR_GREEN, curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_BLUE, curses.COLOR_BLUE, curses.COLOR_BLACK)
	# clear screen
	mapScreen.clear()
	mapScreen.refresh()
	
	

	#Populate levelMap string
#	s2 = ("e e e # # # # e e e e e e e e e e e e e e e e e # # # e e e e e e e e e e e e e e e /"
#		 "e e e # . . # # # # # # # # # # # # e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . # # . . . . . . . . . . # e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . # # . # # # # . # # # . # # # # # # # # . # # # # # # # # # # # # # # # # /"
#		 "e e e # . # # . # e e # . # # # . . . . . . . . . . . . . . . . . . . . . . . . . # /"
#		 "e e e # . # # . # e e # . . . . . # # # # # # # # . # # # # # # # # # # # # # # # # /"
#		 "e e e # . . > . # # # # % # # # # # e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # # # # # # # @ . . # e e e e e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e e e e e e e # . # # # e e e e e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # # # e e e # . # e e e e e e e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . # # e e # . # e e e e e e e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . . # # # # . # # # # # # # # # # # # # # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . # # . . . . . . . . . . . < . . . . . . . # e e e e e e e e e e e e e e e /"
#		 "e e e # . # # . # # # # . # # # . # # # # # # # # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . # # . # e e # . # # # . # e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . # # . # e e # . . . . . # e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # . . . . # # # # . # # # # # e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e # # # # # # # . . . # e e e e e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e e e e e e e # . # # # e e e e e e e e e e # . # e e e e e e e e e e e e e e e /"
#		 "e e e e e e e e e # # # e e e e e e e e e e e e # # # e e e e e e e e e e e e e e e"
#		)

#	s1 = (". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . #/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . #/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . #/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . < . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . > . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e/"
#		 ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . e"
#		)

	dungeon = []
	#initialize our level with the input string
	s = levelGenerator.generateLevel(Gb.windowHeight, Gb.windowWidth, True)
	dungeon.append(Level(s))
	
	for i in range(4):
		s = levelGenerator.generateLevel(Gb.windowHeight, Gb.windowWidth, False)
		dungeon.append(Level(s))
	#initialize our character at an occupiable point on our new map
	for character in dungeon[0].characters:
		if character.symbol == '@':
			player = character
			player.name = "foobar"
			player.currentLevel = 0
	
	logRecord = ["Hello", "Welcome to", "RoguelikeThing"]
	
	#initialize our input character variable
	c = 0

	#while we don't recieve the quit character we keep executing the draw-getcharacter loop
	while chr(c) != 'q' and player.currentHp > 0:
		dungeon[player.currentLevel].clearTileValues()
		#draw our map and handle relevant input
		drawMap(c, mapScreen, player, dungeon[player.currentLevel])
		#wait for a new keystroke
		c = mapScreen.getch()		
		
	#Return the terminal to working order.
	cursesCleanup()

except:
	#If we catch an error it's important to restore the terminal
	#to it's original state or else it will be unusable.
	cursesCleanup()
	#Provide a traceback
	if Gb.traceback:
		traceback.print_exc()
	if Gb.debug:
		print Gb.debug
