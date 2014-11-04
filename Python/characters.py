##########################################################################
#File name: characters.py
#Author: Klemente Gilbert-Espada										
#Description: This file contains the class definitions for the living
#entities inside the dungeon. The Player class and Monster classes are 
#defined here as subclasses of a parent "Character" class
#When more monsters are added to the game, they will be added here
#as classes that inherit from the "Monster" class
##########################################################################
import curses, kcurses
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

	isPlayer = False

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
		self.regenCounter = 0

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
		selfTile = self.getTile()
		selfTile.character = NullCharacter()
		#put ourselves out of the way
		#self.xPos = -1
		#self.yPos = -1
		
		#print out to the screen what happened
		log(killer.name +" killed a " +self.name)

		#remove ourselves from the level we're on's character array
		self.level.characters.remove(self)
		#if the player killed us, give them some experience.
		if killer.isPlayer:
			killer.xp += self.xpValue
		#If we're the player, the game is over
		if self.isPlayer:
			#Revert the terminal to normal.
			kcurses.cursesCleanup()
			#let the player know what happened
			print "sorry you were killed by a " + killer.name
			#don't print a traceback, this isn't a bug
			#exit program
			exit(0)

	#Our pickup function checks to see if our tile has any items on it,
	#and if it does, we grab the first one and add it to our inventory
	#Paramaters: None
	#Returns: Nothing
	def pickup(self):

		currentTile = self.getTile()
		#if the tile we're standing on has items on it, we take one
		if currentTile.contents:
			#add the item to our character's inventory
			self.inventory.append(currentTile.contents[0])
			#remove the item from the tile
			del currentTile.contents[0]


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
		#We assume we can see ourselves
		self.lineOfSight.append(self.getTile())
		if self.isPlayer:
			selfTile = self.getTile()
			selfTile.visible = True
			selfTile.explored = True

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
							if self.isPlayer:
								tile.visible = True
								if not tile.explored:
									tile.explored = True

							#If the tile we're looking at is opaque, we stop walking along this line
							if (tile.terrain.passable == False):
								break

	#Every character should heal naturally over time
	#Every ten turns, a character heals 1 hp
	#Parameters: 	None
	#Returns: 		Nothing
	def healNaturally(self):
		#If we should be healing, we do
		if self.currentHp < self.maxHp:
			#If we've been healing ten turns, 
			#Heal 1 hp
			self.regenCounter += 1
			if self.regenCounter >= 10:
				self.currentHp += 1
				#Start regenerating again
				self.regenCounter = 0

	#This function returns the tile the character is standing on
	#Parameters: 	None
	#Returns: 		The tile the character is currently inhabiting
	def getTile(self):
		#Because this really is a mouthful
		return self.level.levelMap[self.yPos][self.xPos]


#Our monster class defines any character that is not the player
#currently it is a list of AI functions for hostile characters
class Monster (Character):
	#The path list is a stack of directions that the monster has
	#to get it to a target destination
	path = []

	#This is my implementation of the pathfinding
	#algorithm detailed at: 
	#http://www.roguebasin.com/index.php?title=Quick_Pathfinding_in_a_Dungeon
	#Find path should take a target tile,
	#and return a list of directions to get to that tile
	#Parameters:	A target tile
	#Returns: 		nothing
	def findPath(self, target):

		#The origin is the tile our character is standing on.
		origin = self.getTile()
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
		for tile in self.lineOfSight:
			if tile.character.isPlayer:
				return tile
		#Otherwise, don't
		return False

	#The take turn function takes care of everything that this monster does this turn,
	#starting by updating it's line of sight, seeing if it has any changes to it's plan for the turn,
	#and then executing that plan.
	#Parameters: 		None
	#Returns: 			Nothing
	def takeTurn(self):		
		self.healNaturally()
		#Refresh line of sight
		self.getLineOfSight()
		#If we can see the hero, adjust our directions so we're moving towards them
		playerTile = self.canSeeHero()
		if playerTile:
			#Update our path so it points to the hero's nearest square
			self.findPath(playerTile)
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

	isPlayer = True

	def takeTurn(self, c):
		passTurn = False
		level = self.level
		#if our command is a directional key,
	#we move our player
		if c == 65 or c == 56 or chr(c) == 'w':
			self.move([-1, 0])
			passTurn = True

		if c == 66 or c == 50 or chr(c) == 's':
			self.move([1, 0])
			passTurn = True

		if c == 67 or c == 54 or chr(c) == 'd':
			self.move([0, 1])
			passTurn = True

		if c == 68 or c == 52 or chr(c) == 'a':
			self.move([0, -1])
			passTurn = True

		if c == 49:
			self.move([+1, -1])
			passTurn = True

		if c == 51:
			self.move([+1, 1])
			passTurn = True

		if c == 55:
			self.move([-1, -1])
			passTurn = True

		if c == 57: 
			self.move([-1, 1])
			passTurn = True

		#If the command is numpad 5, we wait a turn and do nothing.
		if c == 53:
			passTurn = True

		#If the command is g, we attempt to pick up an item in our tile
		if chr(c) == 'g':
			self.pickup()
			passTurn = True

		#If the command is u, we use a potion in our inventory
		if chr(c) == 'u':
			#If we have any, that is
			if self.inventory:
				self.inventory[0].drink(self)
				passTurn = True

		selfTile = self.getTile()
		
		#If the command is '<', we attempt to go up a staircase
		if chr(c) == '<':
			passTurn = True
			#If we're standing on a staircase that goes up, that is
			if selfTile.terrain.symbol == '<':
				#Remove ourself from the level's array of characters
				level.characters.remove(self)
				#Remove our character from the tile it's on
				selfTile.character = NullCharacter()
				#Set the currentLevel up one (towards 0)
				self.currentLevel -= 1
				#Set our current level to the new level
				self.level = dungeon[self.currentLevel]
				#Set the level for the rest of the function to the new level
				level = self.level
				#Add our character to the new level's character list
				level.characters.append(self)
				
				#Set our player's coordinates to the coordinates of the new
				#staircase we'll be standing on.
				self.yPos = level.DownCoordinates[0]
				self.xPos = level.DownCoordinates[1]

				#update the new tile we're on so it knows we're there
				selfTile = self.getTile()
				selfTile.character = self
			else :
				#The player's trying to do an impossible thing, otherwise
				log("Sorry, you can't go up here")


		#If the command is '>', we attempt to go down a staircase
		if chr(c) == '>':
			passTurn = True
			#If we're standing on a staircase that goes down, that is
			if selfTile.terrain.symbol == '>':
				#Remove ourself from the level's array of characters
				level.characters.remove(self)
				#Remove our character from the tile it's on
				selfTile.character = NullCharacter()
				
				#Set the currentLevel Down one (towards the bottom of the dungeon)
				self.currentLevel += 1
				#Set our current level to the new level
				self.level = dungeon[self.currentLevel]
				#Set the level for the rest of the function to the new level
				level = self.level

				#Add our character to the new level's character list
				level.characters.append(self)

				#Set our player's coordinates to the coordinates of the new
				#staircase we'll be standing on.
				self.yPos = level.UpCoordinates[0]
				self.xPos = level.UpCoordinates[1]

				#update the new tile we're on so it knows we're there
				selfTile = self.getTile()
				selfTile.character = self
			else :
				#The player's trying to do an impossible thing, otherwise
				log("Sorry, you can't go down here")
		if passTurn:
			self.healNaturally()
		return passTurn

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
	isPlayer = False

#Put some messages in our log.
logRecord = ["Hello", "Welcome to", "RoguelikeThing"]
dungeon = []

def drawLog(y, x, screen):
	#Print the last three messages in the log
	if logRecord:
		screen.addstr(y + 2, x, logRecord[len(logRecord)-1])
		screen.addstr(y + 1, x, logRecord[len(logRecord) -2])
		screen.addstr(y, x, logRecord[len(logRecord)-3])

#This is just a tiny wrapper,
#but when you call this function 
#it should output this message to the screen
#in a place that the player can see it
#Parameters: 	(string)The message you want to log.
#Returns: 		Nothing
def log(message):
	logRecord.append(message)

#This grabs the current dungeon from main.py,
#it's basically an artifact of breaking the code up
#into modules, and it lets the player traverse levels
#within its own methods
#parameters: 	an array of levels
#returns: 		nothing
def updateDungeon(newDungeon):
	dungeon.extend(newDungeon)
