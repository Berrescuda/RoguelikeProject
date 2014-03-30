import curses, traceback
import levelGenerator

# For now we'll store some global variables here
class Gb:
	windowHeight = 18
	windowWidth = 36
	debug = []
	traceback = True

# # # # # # # # # # # # # CLASS DEFINITIONS # # # # # # # # # # # # # # # #

#the Character class describes any living entity in the dungeon.
#(for now that only means monsters and the player)
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

	#character's attack power
	power = 1

	#character's line of sight list
	#(This is re-evaluated every turn)
	lineOfSight = []

	#character's inventory (starts empty)
	inventory = []

	#Our initialization function
	#paramaters: level we're initializing on, y position on that level,
	#x position on that level, character name
	#returns nothing
	def __init__(self, level, yPos, xPos):
		#update our personal coordinates
		self.yPos = yPos
		self.xPos = xPos
		#keep track of what level this character is on
		self.level = level
		#add ourself to that level's list of characters
		level.characters.append(self)
		#let the tile we're initializing on know we're here
		level.levelMap[yPos][xPos].character = self

	#The attack function allows one character to attack another.
	#Paramaters: the character to be attacked
	#Returns:	 nothing, although once the function becomes more
	#complicated it may make more sense to return whether or not
	#the attack was successful.
	def attack(self, target):
		#Damage the target character
		target.currentHp -= self.power
		#print what's happening to the output box
		log(self.name +" attacks " + target.name)		
		#check to see if we killed it
		if target.currentHp == 0:
			target.die(self)

	#Our die function removes the character from the map, and updates
	#the different places that need to know it's dead.
	#It also adds experience to the character that killed us.
	#Paramaters: 	character that killed us
	#Returns: 		nothing
	def die(self, killer):
		#Remove our character from the current tile's character attribute.
		self.level.levelMap[self.yPos][self.xPos].character = NullCharacter()
		#put ourselves out of the way
		#self.xPos = -1
		#self.yPos = -1
		
		#print out to the screen what happened
		log(killer.name +" killed a " +self.name)

		#remove ourselves from the level we're on's character array
		self.level.characters.remove(self)
		#if the player killed us, give them some experience.
		if killer == player:
			killer.xp += self.xpValue
		#If we're the player, the game is over
		if self == player:
			#Revert the terminal to normal.
			cursesCleanup()
			#let the player know what happened
			print "sorry you were killed by a " + killer.name
			#don't print a traceback, this isn't a bug
			Gb.traceback = False
			#exit program
			exit(0)

	#Our pickup function checks to see if our tile has any items on it,
	#and if it does, we grab the first one and add it to our inventory
	#Paramaters: None
	#Returns: Nothing
	def pickup(self):
		#so we don't have to type out the whole thing for finding our
		#tile over and over again
		currentTile = self.level.levelMap[self.yPos][self.xPos]
		#if the tile we're standing on has items on it, we take one
		if currentTile.contents:
			#add the item to our character's inventory
			self.inventory.append(currentTile.contents[0])
			#remove the item from the tile
			del self.level.levelMap[self.yPos][self.xPos].contents[0]


	#Our Move function moves our character on the level map.
	#paramaters: direction (tuple indicating movement in the y and x directions),
	#and map of the level we're on
	#returns a brief message if movement isn't possible
	def move(self, direction):
		#Determine which tile we're trying to move onto
		#the first number in our direction tuple
		#tells us how many spaces to move in the y direction.
		targetYPos = self.yPos + direction[0]
		#the second one tells us how many spaces to move in the x direction.
		targetXPos = self.xPos + direction[1]
		#grab the map of the level.
		levelMap = self.level.levelMap
		#once we've found it we set the tile's name to 'target'
		#for the sake of easier reference throughout the function.
		target = levelMap[targetYPos][targetXPos]

		#if the destination we're attempting to move to is blocked, 
		#either by a character or by a wall, we can't move there.
		if target.character.name != "null":
			#if the target tile's character name exists, 
			#we attack it and exit the function.
			self.attack(target.character)
			return 1

		#If our target tile is empty, and we can walk on it, we move onto it.
		if target.terrain.passable:
			#if it's not blocked we move to the destination square
			#starting by clearing out the character from the tile we're currently on
			levelMap[self.yPos][self.xPos].character = NullCharacter()
			#update our personal coordinates to the coordinates of our destination.
			self.yPos += direction[0]
			self.xPos += direction[1]
			#update the destination tile to point to our character
			target.character = self
			return 0
		else:
			#if the destination square *is* blocked, we do nothing
			return 2

	#Our getLineOfSight function populates our character's lineofSight list
	#with tiles that should be visible to the character.
	#This is adapted from the "dumb algorithm" for determining a digital field of view
	#as detailed here: http://www.roguebasin.com/index.php?title=Digital_field_of_view_implementation
	#It's on my to do list to implement one of the smarter algorithms eventually
	#Paramaters: 	none
	#Returns: 		nothing
	def getLineOfSight(self):
		#empty our list
		self.lineOfSight = []
		#for now the only distance that characters are able to see out to is 8 squares,
		#this can and should be updated later.
		distance = 8
		#We're going to repeat the algorithm 8 times, 
		#one for each direction relative to the character.
		for direction in range (8):
			#The goal of this algorithm is to determine every balanced 
			#digital line that can be drawn from the character to viable squares.
			#so we want to repeat Bresenham's line algorithm for every acceptable
			#slope of p/q and every acceptable starting point (eps).
			for q in range (0, distance + 1):
				for p in range (0, q + 1):
				 	for eps in range (0, q + 1):
				 		#After all those for loops, we're ready to walk across our
				 		#digital line until we find an obstruction or we reach the end.
						y = 0
						for x in range(1, distance + 1):
							#This is is the core of Bresenham's line algorithm:
							eps += p
							if (eps >= q):
								eps -= q
								if direction & 2:
									#if our direction is 2, 3, 6 or 7,
									#we're looking towards the top half of the map,
									#so y should work it's way towards the top.
									y -= 1
								else:
									#otherwise we're looking towards the bottom half of the 
									#map, and y should increment.
									y += 1
							#
							if direction & 1:
								#If our direction is odd, we're looking left instead of right,
								#so we're looking at coordinates towards x == 0 instead of 
								#x == mapWidth
								x = -x

							if direction &4:
								#If our direction is 4, 5, 6, or 7,
								#we reverse our x and y coordinate (this lets us
								#draw lines as though the x axis was the y axis and visa versa,
								#which makes it possible to fill out the rest of our field of view
								tile = self.level.levelMap[x + self.yPos][y + self.xPos]
							else: #Then one way or another we find the tile we're standing on
								tile = self.level.levelMap[y + self.yPos][x + self.xPos]
							
							#then we append the tile to our character's line of sight.
							self.lineOfSight.append(tile)

							#If we're the point of view character, we highlight tiles in our line of sight
							if self == player:
								tile.visible = True

							#If the tile we're looking at is opaque, we stop walking along this line
							if (tile.terrain.passable == False):
								break

#Our monster class defines any character that is not the player
#currently it is a list of AI functions for hostile characters
class Monster (Character):
	#The path list is a stack of directions that the monster has
	#to get it to a target destination
	path = []

	#This will be my attempted implementation of the pathfinding
	#algorithm detailed at: 
	#http://www.roguebasin.com/index.php?title=Quick_Pathfinding_in_a_Dungeon
	#Find path should take a target tile,
	#and return a list of directions to get to that tile
	#Parameters:	A target tile
	#Returns: 		nothing
	def findPath(self, target):

		#The origin is the tile our character is standing on.
		origin = self.level.levelMap[self.yPos][self.xPos]
		#Give the origin a pathvalue that makes it recognizable
		#to our algorithm.
		origin.pathValue = -1

		#Origin is now the current square.
		currentTile = origin
		#We clear off all of the pathValues that may be left over
		#from all tiles on the level.
		self.level.clearTileValues()
		#We have a list of unexplored tiles that will be queued
		#to be explored.
		unexploredTiles = []
		#we have a list of directions that we will set our character's
		#path to, so that they can follow them to the target tile.
		directions = []

		while(currentTile != target):
			#make a list of adjacent tiles, and go through them to see
			#which ones we've explored
			adjTiles = currentTile.listAdjacentTiles(self.level.levelMap)
			for tile in adjTiles:
				#If we're not going backwards specifically and 
				#the tile is something we can walk on, we assign it
				#a value
				if tile.terrain.passable and tile != origin:
						#If the tile's path value is 0, the tile is unexplored.
						if tile.pathValue == 0:
							#If we're just starting, we have to initialize
							#the adjacent tiles at one (if we set the origin tile to 0
								#it causes problems)
							if currentTile == origin:
								tile.pathValue = 1
							else:
								#We set the adjacent 0's to our current tile's path value
								#+1 this way we know how far every square is from the player
								tile.pathValue = currentTile.pathValue + 1
							
							#We put all tiles who we've given a path value to
							#into the unexplored tiles list, if we don't find
							#our target next to us, we'll look through those tiles
							#and continue searching.
							unexploredTiles.append(tile)

							#If the tile we're iterating over is the target, we're done
							#with this part of the algorithm
							if tile == target:
								currentTile = tile
								break
			#While we haven't found the target tile, and there are still unexplored
			#tiles left, iterate over them.
			if unexploredTiles and currentTile != target:
				currentTile = unexploredTiles.pop(0)
		
		#Then we start at the target tile, and work our way backwards
		#to find the origin, pushing the direction our character will
		#have to go onto the directions list
		while(currentTile != origin):
			#Check adjacent tiles
			adjTiles = currentTile.listAdjacentTiles(self.level.levelMap)
			for tile in adjTiles:
				#find a square adjacent to the current square that 
				#is one fewer spaces away from the origin square
				if tile.pathValue == currentTile.pathValue - 1:
					#if we haven't found the origin, push
					#another set of directions onto the list
					if tile == origin or tile.pathValue > 0:
						y = currentTile.yPos - tile.yPos 
						x = currentTile.xPos - tile.xPos
						directions.append((y, x))
						#If the tile is the origin, we update
						#our character's directions, and quit the function
						if tile == origin:
							self.path = directions
							return 1
						#Otherwise we set this tile to the current tile, 
						#and start the process again
						currentTile = tile
						break
	
	#canSeeHero is a function that returns true if the player character
	#is within this monster's line of sight
	#Parameters: 		None
	#Returns: 			Nothing
	def canSeeHero(self):
		#If the player's tile is in line of sight, return true.
		if player.level.levelMap[player.yPos][player.xPos] in self.lineOfSight:
			return True
		#Otherwise, don't
		return False

	#The take turn function takes care of everything that this monster does this turn,
	#starting by updating it's line of sight, seeing if it has any changes to it's plan for the turn,
	#and then executing that plan.
	#Parameters: 		None
	#Returns: 			Nothing
	def takeTurn(self):
		#Refresh line of sight
		self.getLineOfSight()
		#If we can see the hero, adjust our directions so we're moving towards them
		if self.canSeeHero():
			#Update our path so it points to the hero's nearest square
			self.findPath(player.level.levelMap[player.yPos][player.xPos])
		#If our path isn't empty, move along it.
		if self.path:
			self.move(self.path.pop())

#The Player is the class of character played by the character.
#Not a lot seperates it from monsters as of yet.
class Player (Character):
	#Experience Points
	xp = 0
	#The player symbol is '@'
	symbol = '@'

#The SpaceGoblin is currently our only enemy,
#But as the game develops we should add scores more.
#They're simple and pretty weak compared to the character
class SpaceGoblin (Monster):
	#Yes, all of them are named "Space Goblin"
	name = "Space Goblin"
	#They will be represented by a red g for now.
	#(Color is set in the character template)
	symbol = 'g'
	#They have half of the hp of the player.
	maxHp = 5
	currentHp = maxHp
	#They don't do magic. But to be fair, no one does yet.
	maxMp = 0
	currentMp = 0
	#They're worth the least possible experience.
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

def initCurses():
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
	return mapScreen

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

	#Print the current level of the dungeon the player is on
	screen.addstr(y + 5, x, "Current Level: " + str(player.currentLevel))

	#Print the items in the character's inventory.
	screen.addstr(y + 6, x, "Inventory:")
	for i in range(len(player.inventory)):
		screen.addstr(y + 7 + i, x, player.inventory[i].name)

 # # # # # # # # # # # # # MAIN BODY OF CODE # # # # # # # #
try:
	#Initialize our screen
	mapScreen = initCurses()

	#Set up an empty array that will hold the levels of our dungeon.
	dungeon = []

	#initialize our first level with the function from levelGenerator.
	#The boolean at the end of the function call 
	#tells the generator to put the player on the level.
	s = levelGenerator.generateLevel(Gb.windowHeight, Gb.windowWidth, True)
	dungeon.append(Level(s))
	
	#Generate three more levels for our dungeon.
	for i in range(4):
		s = levelGenerator.generateLevel(Gb.windowHeight, Gb.windowWidth, False)
		dungeon.append(Level(s))

	#Find our player in the first level of the dungeon,
	#and make them easier to find in the future.
	for character in dungeon[0].characters:
		if character.symbol == '@':
			player = character
			#Initialize certain player variables
			player.name = "foobar"
			player.currentLevel = 0
	
	#Put some messages in our log.
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
	#Print a debug message if one is present
	if Gb.debug:
		print Gb.debug
