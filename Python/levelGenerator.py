##########################################################################
#File name: levelGenerator.py
#Author: Klemente Gilbert-Espada										
#Description: This file contains functions for "drawing" a level of the 
#dungeon. 
##########################################################################

from random import randint
import traceback

#The room class defines a room inside the level
#It contains coordinates and the dimensions of the room to be created,
#as well as information about whether or not the room is connected to 
#the rest of the dungeon
class Room:
	#width of the room
	width = 0
	#height of the room
	height = 0
	#x position of the upper left hand corner of the room
	xPos = 0
	#y position of the upper left hand corner of the room
	yPos = 0
	#boolean showing whether or not the room is connected
	#to the rest of the dungeon
	connected = False

	#when a room is initialized, it's dimensions are randomized and placed 
	#somewhere randomly on the map, with a random width and height
	#Then the room is "drawn" on the map
	#parameters: 	width and height of the map we're drawing, 
	# 				the map we're drawing on.
	#returns: 		nothing
	def __init__(self, mapWidth, mapHeight, levelMap):
		#randomize width
		self.width = randint(3, 6)
		#randomize height
		self.height = randint(3, 6)

		#randomize position
		self.xPos = randint(0, mapWidth -self.width)
		self.yPos = randint(0, mapHeight - self.height)

		#iterate over the map array and plant a "." on spaces
		#where our room exists
		for y in range(self.height):
			for x in range(self.width):
				#set string at coordinates to represent empty floor
				levelMap[self.yPos + y][self.xPos + x] = '.'

	#place in room puts an object in an unocupied square in our room
	#parameters: 	symbol of the object we're placing,
	#				map of the level we're placing it on
	#returns: 		nothing
	def placeInRoom(self, symbol, levelMap):
		#While we haven't found an empty square, keep trying to
		success = False
		while not success:
			#randomize the position of the object
			y = self.yPos + randint(1, self.height -1)
			x = self.xPos + randint(1, self.width -1)

			#check to see if this square is occupied
			if levelMap[y][x] == '.':
				#if it isn't, set the square to display our object
				levelMap[y][x] = symbol
				#and end our loop
				success = True

	#This function still feels a tiny bit clunky
	#There's a hole in my logic somewhere that sometimes generates weird artifacts
	#(very small rooms, a disconnected origin room)
	#What the function does is take a room that's not yet connected to the dungeon,
	#and draw a corridor to a room that is. It does this by drawing a corridor 
	#in one direction towards the room until the corridor is lined up with 
	#the target room, then travelling along the second axis until we have 
	#a corridor that goes from one room to the other.
	#parameters: 		room to draw a corridor to, the map we're drawing on
	#returns: 			True
	def connect(self, room, levelMap):

		# # # # # ORIENTATION # # # # #
		#if the room we're trying to draw a corridor to is on our right,
		#we will be going east
		if room.xPos > self.xPos:
			xDirection = 1
		else:
			#otherwise we'll be going west
			xDirection = -1

		#on a similar note, if the room we're connecting to is above us, we're going north
		if room.yPos < self.yPos:
			yDirection = -1
		else: 
			#and if it's below us we're going south
			yDirection = 1

		# # # # # ENTRANCE PLACEMENT # # # # # 
		#we take a 50/50 shot of leaving from the north/south wall, or from the east/west wall
		startDirection = coinFlip()
		#True indicates that we start in the y direction
		if startDirection:
			#if the coinflip turns up y, we leave from the wall corresponding to the direction
			#we'll be travelling in the y direction
			entrance = (yDirection)	
			if entrance == -1:
				#if the entrance is on the north side, we set the initial square for the entrance
				#somewhere randomly on the north wall
				corridor = [self.yPos, randint(self.xPos, self.xPos + self.width)]
			
			else:
				#otherwise we'll initialize the entrance to the corridor at a random point on the south wall
				corridor = [self.yPos + self.height, randint(self.xPos, self.xPos + self.width)]

		#If the coinflip indicates otherwise, we'll start instead by travelling in the x direction
		else:
			#we will leave from the east or west wall
			entrance = (xDirection)
			if entrance == -1:
				#set the entrance somewhere randomly on the west wall
				corridor = [randint(self.yPos, self.yPos + self.height), self.xPos]
			else:
				#set the entrance somewhere random on the east wall
				corridor = [randint(self.yPos, self.yPos + self.height), self.xPos + self.width]
		
		# # # # # BUILD CORRIDOR AND WRAP UP # # # # 
		#Then we build our corridor from our start room to our target room.
		buildCorridor(corridor, room, levelMap, xDirection, yDirection, startDirection)

		#we are now connected to the other room, and by extension, the rest of the dungeon
		self.connected = True
		return True

#The following function takes a square (of floor, usually) on the map,
#and fills in all empty neighbors with wall "#"
#So that if you have a section of dungeon like this:
#
# 			. . . . 
# 			. . . .
# 			. . . . . . . 
#
# If each floor square has wallAdjacentSpaces() on it,
# that section of dungeon should look like this afterward:
#		  # # # # # #
# 		  # . . . . #
# 		  # . . . . # # # #
# 		  # . . . . . . . #
# 		  # # # # # # # # #
#
#Parameters: 	The x and y coordinates of the square who's neighbor's
#				we'll be looking at
#Returns: 		a code specifying how the function went
# 				a 1 if the square we start at should actually be a wall,
#				a 0 if everything went according to plan
def wallAdjacentSpaces(x, y, levelMap):
		#we have a list that holds tuples with the coordinates
		#of squares adjacent to the one at coordinates x, y
		#(but it starts empty)
		adjacentSpaces = []
		#our shouldBeWall variable makes sure that
		#each of the following if statements is true
		#otherwise this square is on a border of the map
		#itself, and if it's anything but a wall, the
		#player will be able to walk off of the level,
		#which obviously is not desired behavior and would
		#cause the game to crash
		shouldBeWall = 8

		#if y is greater than 0, there is a square above us
		if y > 0:
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y - 1, x])		#Up
			#decrement our security measure
			shouldBeWall -= 1
			
		#if y is less than the height of the map,
		#a square exists below us
		if y < len(levelMap) - 1:					#Down
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y + 1, x])
			#decrement our security measure
			shouldBeWall -= 1

		#if x is less than the width of the map,
		#a square exists to our right
		if x < len(levelMap[y]) - 1:
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y, x + 1])		#Right
			#decrement our security measure
			shouldBeWall -= 1

		#if x is greater than 0, there is a tile to our left
		if x > 0:
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y, x - 1])		#Left
			shouldBeWall -= 1

		#check if a tile exists diagonally to the right and down
		if y < len(levelMap) - 1 and x < len(levelMap[y]) - 1:
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y + 1, x + 1])	#DownRight
			#decrement our security measure
			shouldBeWall -= 1
		
		#check if a tile exists diagonally to the right and up
		if y > 0 and x < len(levelMap[y]) - 1:
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y -1, x + 1])		#UpRight
			#decrement our security measure
			shouldBeWall -= 1
		
		#check if a tile exists diagonally up and to the left
		if y > 0 and x > 0:
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y - 1, x - 1])	#UpLeft
			#decrement our security measure
			shouldBeWall -= 1

		#check if a tile exists diagonally down and to the left
		if y < len(levelMap) - 1 and x > 0:
			#add the coordinates of our tile to our list
			adjacentSpaces.append([y + 1, x - 1])	#DownLeft
			#decrement our security measure
			shouldBeWall -= 1

		if shouldBeWall > 0:
			#if should be wall has been decremented any less than 8 times,
			#this square needs to be a wall or it will break the game
			levelMap[y][x] = '#'
			#let the caller of the function know if they want to
			return 1

		#iterate over every adjacent space to our square
		for space in adjacentSpaces:
			#if the adjacent space is empty (not a ".")
			#put a wall there
			if levelMap[space[0]][space[1]] == '':
				levelMap[space[0]][space[1]] = '#'
		
		#return an all clear
		return 0

#our coinflip function returns true or false with 50/50 odds
#just a cute little function that might shorten some areas of the code
#parameters: 	none
#returns: 		the result of the coin flip
def coinFlip():
	if randint(0,1) == 1:
		return True
	else: 
		return False

#The buildCorridor function builds a corridor from one room to another
#Parameters:		the starting position of the corridor
# 					the room we're building towards
#					the map we're drawing on
#					the direction we will travel on the x axis
#					the direction we will travel on the y axis
#					The direction we will start off going in
#
#Returns: 			Nothing

def buildCorridor(corridor, target, levelMap, xDirection, yDirection, startInYDirection):
	#If we start off in the y direction:
	if startInYDirection:
		#go up or down until we lie some distance from the target room
		#along the x axis
		corridor = goY(corridor, target, levelMap, yDirection)
		#travel along the x axis until we collide with the room
		goX(corridor, target, levelMap, xDirection)
	#otherwise we start off travelling in the x direction
	else:
		#we go left or right until we lie along the y axis with the target room
		corridor = goX(corridor, target, levelMap, xDirection)
		#travel along the x axis until we collide with the room
		goY(corridor, target, levelMap, yDirection)

#These two functions could probably be conflated
#into one function with proper generalization of the parameters

#The goY function draws a square, and then travels along the y axis one square
#repeating this process until it reaches the target value of y.
#effectively drawing a hallway up or down until a given point.
#parameters:	the position we start at, 
#				the position we're going to, 
# 				the map we're drawing on
#returns: 		the position of the corridor when we're done
def goY(corridor, target, levelMap, direction):
	#while we're not directly to the left or right of the target room
	while corridor[0] > (target.yPos + target.height + direction):
		#plant a square of empty floor
		levelMap[corridor[0]][corridor[1]] = '.'
		#travel in the specified direction
		corridor[0] += direction
	#when we're done, return the new position of the end of the corridor
	return corridor

#The goX function draws a square, and then travels along the x axis one square
#repeating this process until it reaches the target value of x.
#effectively drawing a hallway to the left or right until a given point.
#parameters:	the position we start at, 
#				the position we're going to, 
# 				the map we're drawing on
#returns: 		the position of the corridor when we're done
def goX(corridor, target, levelMap, direction):
	#While we're not directly beneath or above the target room
	while corridor[1] < (target.xPos + direction):
		#plant a square of empty floor
		levelMap[corridor[0]][corridor[1]] = '.'
		#travel in the specified direction
		corridor[1] += direction
	#when we're done, return the new position of the end of the corridor
	return corridor

#Our levelMapToString function parses our level map array and produces
#a string that can be read by the level initializer in mapObjects.py
#parameters: 		An array representing our level
#returns: 			a string representing our map
def levelMapToString(levelMap):
	#Initialize our empty map string
	mapString = ''
	#Iterate along the y axis
	for y in range(len(levelMap)):
		#Iterate along the x axis
		for x in range(len(levelMap[y])):
			#If the square is empty,
			#add a character that signifies that
			if levelMap[y][x] == '':
				#add the relevant character to our string
				levelMap[y][x] = 'e'
				#add a space for aesthetic effect
			mapString += levelMap[y][x] + ' '
		#at the end of each row, we append a slash, to signify
		#the row being over, allowing the mapstring parser
		#to break the string up into rows and columns
		mapString += '/'
	#return the completed string
	return mapString

#The addWalls function goes through the map and turns all
#tiles that should be walls into walls
#parameters: 	the map we're drawing on
#returns: 		nothing
def addWalls(levelMap):
	#start from the top and work towards the bottom
	y = 0
	#iterate over the rows on our map
	for row in levelMap:
		#start at the left side, work towards the right
		x = 0
		#iterate over the spaces in the row
		for space in row:
			#if the space is a floor, check adjacent
			#spaces to see if they should be walls
			if space == '.':
				wallAdjacentSpaces(x, y, levelMap)
			#next square
			x += 1
		#next row
		y += 1

#Our generate level function creates an appropriate level string
#with connected rooms and monsters and items sprinkled around at random,
#and then returns the level string
#parameters: 		width of the level to make, height of the level to make,
# 					whether or not this is the first level of the map
#returns: 			a string representing the map we generate
def generateLevel(mapWidth, mapHeight, start):
	#Initialize our map list
	levelMap = []
	#initialize our list of rooms that haven't been connected to each other yet
	unconnectedRooms = []
	#initialize our list of rooms that have been connected to each other
	connectedRooms = []

	#Generate rows of the map
	for y in range(mapHeight):
		#initialize our row
		row = []
		#generate squares of map
		for x in range(mapWidth):
			#append an empty square (these will get filled out later)
			row.append('')
		#Add our row to our array
		levelMap.append(row)
	#generate an amount of rooms on the level
	roomsOnLevel = randint(7, 13)
	#for each room that we're going to generate
	for i in range(roomsOnLevel):
		#initialize the room
		newRoom = Room(mapWidth, mapHeight, levelMap)
		#add it to our list of unconnected rooms
		unconnectedRooms.append(newRoom)

	#select our first room and label it as connected,
	#now we'll have a room to build our dungeon corridors off of
	connectedRooms.append(unconnectedRooms.pop())
	connectedRooms[0].connected = True

	#while there are rooms that aren't connected to the rest of the dungeon
	while unconnectedRooms:
		#pop a room off of our list of unconnected rooms
		room = unconnectedRooms.pop()
		#we execute the connect function in a try block, because it occasionally fails
		try:
			#connect our unconnected room to the rest of the dungeon
			room.connect(connectedRooms[randint(0, len(connectedRooms)) -1], levelMap)
			#push our connected room into our list of connected rooms
			connectedRooms.append(room)
		except:
			#if our connect() call fails, we just put our room back in the list of 
			#unconnected rooms
			unconnectedRooms.append(room)
	#we now have a basic shell of the dungeon
	#composed of "." spaces and empty spaces
	#we call addWalls() to fill in all of the empty spaces
	#that are adjacent to "." spaces with "#"
	addWalls(levelMap)
	
	#if this is the first level of the dungeon, we dump the player in the first room
	#we generated (because that's easy)
	if start:
		connectedRooms[0].placeInRoom("@", levelMap)
	
	#put an up staircase on every level
	connectedRooms[randint(0, len(connectedRooms)) - 1].placeInRoom("<", levelMap)
	#put a down staircase on every level
	connectedRooms[randint(0, len(connectedRooms)) - 1].placeInRoom(">", levelMap)
	
	#for every room
	for room in connectedRooms:
		#25% chance of the room having a goblin in it
		if randint(1, 4) == 4:
				room.placeInRoom("g", levelMap)
		#8% chance of the room having a hobo in it
		if randint(1,100) <= 8:
			room.placeInRoom("h", levelMap)
		#one in seven chance of a potion being put in the room.
		if randint(1, 7) == 7:
			room.placeInRoom("%", levelMap)
			
	#return the completed string
	return levelMapToString(levelMap)
