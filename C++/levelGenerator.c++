//File name: levelGenerator.py
//Author: Klemente Gilbert-Espada										
//Description: This file contains functions for "drawing" a level of the 
//dungeon. 

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <vector>
#include <deque>
#include <stack>
#include <string>

using namespace std;

struct Tuple{
int xPos;
int yPos;
};

//The room class defines a room inside the level
//It contains coordinates and the dimensions of the room to be created,
//as well as information about whether or not the room is connected to 
//the rest of the dungeon
struct Room{
	//width of the room
	int width;
	//height of the room
	int height;
	//x position of the upper left hand corner of the room
	int xPos;
	//y position of the upper left hand corner of the room
	int yPos;
	//boolean showing whether or not the room is connected
	//to the rest of the dungeon
	bool connected;

	Room(int, int, vector< vector<char> >);
	int placeInRoom(char, vector< vector<char> >);
	bool connect(Room, vector< vector<char> >);
};

struct Entrance{
int xPos;
int yPos;
};
//when a room is initialized, it's dimensions are randomized and placed 
//somewhere randomly on the map, with a random width and height
//Then the room is "drawn" on the map
//parameters: 	width and height of the map we're drawing, 
// 				the map we're drawing on.
//returns: 		nothing
Room::Room(int mapWidth, int mapHeight, vector< vector<char> > levelMap){
	connected = false;

	//randomize width
	width = rand()% 3 + 3;
	//randomize height
	height = rand()% 3 + 3;

	//randomize position
	xPos = rand()% (mapWidth - width);
	yPos = rand()% (mapHeight - height);

	//iterate over the map array and plant a "." on spaces
	//where our room exists
	for (int y = 0; y < height; y++)
		for (int x = 0; x < width; x++)
			//set string at coordinates to represent empty floor
			levelMap[yPos + y][xPos + x] = '.';
}

//place in room puts an object in an unocupied square in our room
//parameters: 	symbol of the object we're placing,
//				map of the level we're placing it on
//returns: 		nothing
int Room::placeInRoom(char symbol, vector< vector<char> > levelMap){
	//While we haven't found an empty square, keep trying to
	bool success = false;
	while (!success){
		//randomize the position of the object
		int y = yPos + rand()% (height - 1) + 1;
		int x = xPos + rand()% (width -1) + 1;

		//check to see if this square is occupied
		if (levelMap[y][x] == '.'){
			//if it isn't, set the square to display our object
			levelMap[y][x] = symbol;
			//and end our loop
			success = true;
		}
	}
}

//The following function takes a square (of floor, usually) on the map,
//and fills in all empty neighbors with wall "//"
//So that if you have a section of dungeon like this:
//
// 			. . . . 
// 			. . . .
// 			. . . . . . . 
//
// If each floor square has wallAdjacentSpaces() on it,
// that section of dungeon should look like this afterward:
//		  // // // // // //
// 		  // . . . . //
// 		  // . . . . // // // //
// 		  // . . . . . . . //
// 		  // // // // // // // // //
//
//Parameters: 	The x and y coordinates of the square who's neighbor's
//				we'll be looking at
//Returns: 		a code specifying how the function went
// 				a 1 if the square we start at should actually be a wall,
//				a 0 if everything went according to plan
int wallAdjacentSpaces(int x, int y, vector< vector<char> > levelMap){
		//we have a list that holds tuples with the coordinates
		//of squares adjacent to the one at coordinates x, y
		//(but it starts empty)
		vector <Tuple> adjacentSpaces;
		//our shouldBeWall variable makes sure that
		//each of the following if statements is true
		//otherwise this square is on a border of the map
		//itself, and if it's anything but a wall, the
		//player will be able to walk off of the level,
		//which obviously is not desired behavior and would
		//cause the game to crash
		int shouldBeWall = 8;

		//if y is greater than 0, there is a square above us
		if (y > 0){
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y - 1, x});	//Up
			//decrement our security measure
			shouldBeWall -= 1;
		}
			
		//if y is less than the height of the map,
		//a square exists below us
		if (y < levelMap.size() - 1){					//Down
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y + 1, x});
			//decrement our security measure
			shouldBeWall -= 1;
		}

		//if x is less than the width of the map,
		//a square exists to our right
		if (x < levelMap[y].size() - 1){
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y, x + 1});		//Right
			//decrement our security measure
			shouldBeWall -= 1;
		}
		//if x is greater than 0, there is a tile to our left
		if(x > 0){
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y, x - 1});	//Left
			shouldBeWall -= 1;
		}
		//check if a tile exists diagonally to the right and down
		if (y < levelMap.size() - 1 && x < levelMap[y].size() - 1){
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y + 1, x + 1});	//DownRight
			//decrement our security measure
			shouldBeWall -= 1;
		}
		
		//check if a tile exists diagonally to the right and up
		if (y > 0 && x < levelMap[y].size() - 1){
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y -1, x + 1});		//UpRight
			//decrement our security measure
			shouldBeWall -= 1;
		}
		
		//check if a tile exists diagonally up and to the left
		if(y > 0 && x > 0){
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y - 1, x - 1});	//UpLeft
			//decrement our security measure
			shouldBeWall -= 1;
		}
		//check if a tile exists diagonally down and to the left
		if (y < levelMap.size() - 1 && x > 0){
			//add the coordinates of our tile to our list
			adjacentSpaces.push_back({y + 1, x - 1});	//DownLeft
			//decrement our security measure
			shouldBeWall -= 1;
		}

		if (shouldBeWall > 0){
			//if should be wall has been decremented any less than 8 times,
			//this square needs to be a wall or it will break the game
			levelMap[y][x] = '#';
			//let the caller of the function know if they want to
			return 1;
		}

		//iterate over every adjacent space to our square
		for(int i = 0; i < adjacentSpaces.size(); i++)
				//if the adjacent space is empty (not a ".")
				//put a wall there
				if (levelMap[adjacentSpaces[i].yPos][adjacentSpaces[i].xPos] == NULL)
					levelMap[adjacentSpaces[i].yPos][adjacentSpaces[i].xPos] = '#';
			
			//return an all clear
			return 0;
}

//our coinflip function returns true or false with 50/50 odds
//just a cute little function that might shorten some areas of the code
//parameters: 	none
//returns: 		the result of the coin flip
bool coinFlip(){
	if (rand() % 1)
		return true;
	else
		return false;
}

//These two functions could probably be conflated
//into one function with proper generalization of the parameters

//The goY function draws a square, and then travels along the y axis one square
//repeating this process until it reaches the target value of y.
//effectively drawing a hallway up or down until a given point.
//parameters:	the position we start at, 
//				the position we're going to, 
// 				the map we're drawing on
//returns: 		the position of the corridor when we're done
Entrance goY(Entrance corridor, Room target, vector< vector<char> > levelMap, int direction){
	//while we're not directly to the left or right of the target room
	while (corridor.yPos > (target.yPos + target.height + direction)){
		//plant a square of empty floor
		levelMap[corridor.yPos][corridor.xPos] = '.';
		//travel in the specified direction
		corridor.yPos += direction;
	}
	//when we're done, return the new position of the end of the corridor
	return corridor;
}
//The goX function draws a square, and then travels along the x axis one square
//repeating this process until it reaches the target value of x.
//effectively drawing a hallway to the left or right until a given point.
//parameters:	the position we start at, 
//				the position we're going to, 
// 				the map we're drawing on
//returns: 		the position of the corridor when we're done
Entrance goX(Entrance corridor, Room target, vector< vector<char> > levelMap, int direction){
	//while we're not directly to the left or right of the target room
	while (corridor.xPos > (target.xPos + target.width + direction)){
		//plant a square of empty floor
		levelMap[corridor.yPos][corridor.xPos] = '.';
		//travel in the specified direction
		corridor.yPos += direction;
	}
	//when we're done, return the new position of the end of the corridor
	return corridor;
}

//The buildCorridor function builds a corridor from one room to another
//Parameters:		the starting position of the corridor
// 					the room we're building towards
//					the map we're drawing on
//					the direction we will travel on the x axis
//					the direction we will travel on the y axis
//					The direction we will start off going in
//
//Returns: 			Nothing

void buildCorridor(Entrance corridor, Room target, vector< vector<char> > levelMap, int xDirection, int yDirection, bool startInYDirection){
	//If we start off in the y direction:
	if (startInYDirection){
		//go up or down until we lie some distance from the target room
		//along the x axis
		corridor = goY(corridor, target, levelMap, yDirection);
		//travel along the x axis until we collide with the room
		goX(corridor, target, levelMap, xDirection);
	//otherwise we start off travelling in the x direction
	}else{
		//we go left or right until we lie along the y axis with the target room
		corridor = goX(corridor, target, levelMap, xDirection);
		//travel along the x axis until we collide with the room
		goY(corridor, target, levelMap, yDirection);
	}
}

//Our levelMapToString function parses our level map array and produces
//a string that can be read by the level initializer in mapObjects.py
//parameters: 		An array representing our level
//returns: 			a string representing our map
string levelMapToString(vector< vector<char> > levelMap){
	//Initialize our empty map string
	string mapString;
	//Iterate along the y axis
	for (int y = 0; y < levelMap.size(); y++){
		//Iterate along the x axis
		for(int x = 0; x < levelMap.size(); x++){
			//If the square is empty,
			//add a character that signifies that
			if (levelMap[y][x] == NULL)
				//add the relevant character to our string
				levelMap[y][x] = 'e';
				//add a space for aesthetic effect
			mapString += levelMap[y][x] + ' ';
		//at the end of each row, we push a slash, to signify
		//the row being over, allowing the mapstring parser
		//to break the string up into rows and columns
		mapString += '\n';
		}
	}
	//return the completed string
	return mapString;
}
//The addWalls function goes through the map and turns all
//tiles that should be walls into walls
//parameters: 	the map we're drawing on
//returns: 		nothing
void addWalls(vector< vector<char> > levelMap){
	//iterate over the rows on our map
	for (int y = 0; y < levelMap.size(); y++)
		//iterate over the spaces in the row
		for (int x = 0; x < levelMap[y].size(); x++)
			//if the space is a floor, check adjacent
			//spaces to see if they should be walls
			if (levelMap[y][x] == '.')
				wallAdjacentSpaces(x, y, levelMap);
	
}

//This function still feels a tiny bit clunky
//There's a hole in my logic somewhere that sometimes generates weird artifacts
//(very small rooms, a disconnected origin room)
//What the function does is take a room that's not yet connected to the dungeon,
//and draw a corridor to a room that is. It does this by drawing a corridor 
//in one direction towards the room until the corridor is lined up with 
//the target room, then travelling along the second axis until we have 
//a corridor that goes from one room to the other.
//parameters: 		room to draw a corridor to, the map we're drawing on
//returns: 			True
bool Room::connect(Room room, vector< vector<char> > levelMap){

	int yDirection;
	int xDirection;
	// // // // // ORIENTATION // // // // //
	//if the room we're trying to draw a corridor to is on our right,
	//we will be going east
	if (room.xPos > xPos)
		xDirection = 1;
	else
		//otherwise we'll be going west
		xDirection = -1;

	//on a similar note, if the room we're connecting to is above us, we're going north
	if (room.yPos < yPos)
		yDirection = -1;
	else
		//and if it's below us we're going south
		yDirection = 1;

	// // // // // ENTRANCE PLACEMENT // // // // // 
	//we take a 50/50 shot of leaving from the north/south wall, or from the east/west wall
	bool startDirection = coinFlip();
	//True indicates that we start in the y direction

	Entrance corridor; //Our corridor's starting point

	if (startDirection){
		//if the coinflip turns up y, we leave from the wall corresponding to the direction
		//we'll be travelling in the y direction
		int entrance = yDirection;	
		if (entrance == -1)
			//if the entrance is on the north side, we set the initial square for the entrance
			//somewhere randomly on the north wall
			corridor = {yPos, rand()% (xPos + width) + xPos};

		else
			//otherwise we'll initialize the entrance to the corridor at a random point on the south wall
			corridor = {yPos + height, rand()% (xPos + width) + xPos};
	}
	//If the coinflip indicates otherwise, we'll start instead by travelling in the x direction
	else{
		//we will leave from the east or west wall
		int entrance = xDirection;
		if (entrance == -1)
			//set the entrance somewhere randomly on the west wall
			corridor = {rand()% (yPos + height) + yPos, xPos};
		else
			//set the entrance somewhere random on the east wall
			corridor = {rand()% (yPos + height) + yPos, xPos + width};
	}
	// // // // // BUILD CORRIDOR AND WRAP UP // // // // 
	//Then we build our corridor from our start room to our target room.
	buildCorridor(corridor, room, levelMap, xDirection, yDirection, startDirection);

	//we are now connected to the other room, and by extension, the rest of the dungeon
	connected = true;
	return true;
}

//Our generate level function creates an appropriate level string
//with connected rooms and monsters and items sprinkled around at random,
//and then returns the level string
//parameters: 		width of the level to make, height of the level to make,
// 					whether or not this is the first level of the map
//returns: 			a string representing the map we generate
string generateLevel(int mapWidth, int mapHeight, bool start){
	srand(time(NULL));
	//Initialize our map array
	vector< vector<char> > levelMap;
	levelMap.resize(mapHeight * 2);
	for(int i = 0; i < mapHeight; i++)
		levelMap[i].resize(sizeof(char) * mapWidth);

	for(int i = 0; i < mapHeight; i++)
		for(int j = 0; j < mapWidth; j++)
			levelMap[i][j] = ' ';

	//initialize our list of rooms that haven't been connected to each other yet
	stack <Room> unconnectedRooms;
	//initialize our list of rooms that have been connected to each other
	vector <Room> connectedRooms;

	int roomsOnLevel = rand()% 6 + 7;
	//for each room that we're going to generate
	for (int i = 0; i < roomsOnLevel; i++){
		//initialize the room
		Room newRoom = Room(mapWidth, mapHeight, levelMap);
		//add it to our list of unconnected rooms
		unconnectedRooms.push(newRoom);
	}

	//select our first room and label it as connected,
	//now we'll have a room to build our dungeon corridors off of
	connectedRooms.push_back(unconnectedRooms.top());
	unconnectedRooms.pop();
	connectedRooms[connectedRooms.size() - 1].connected = true;

	//while there are rooms that aren't connected to the rest of the dungeon
	while (!unconnectedRooms.empty()){
		//pop a room off of our list of unconnected rooms
		Room room = unconnectedRooms.top();

		//connect our unconnected room to the rest of the dungeon
		if (room.connect(connectedRooms[rand()% connectedRooms.size() -1], levelMap)){
			//push our connected room into our list of connected rooms
			connectedRooms.push_back(room);
			unconnectedRooms.pop();
		}
	}
	//we now have a basic shell of the dungeon
	//composed of "." spaces and empty spaces
	//we call addWalls() to fill in all of the empty spaces
	//that are adjacent to "." spaces with "//"
	addWalls(levelMap);
	
	//if this is the first level of the dungeon, we dump the player in the first room
	//we generated (because that's easy)
	if (start)
		connectedRooms[0].placeInRoom('@', levelMap);
	
	//put an up staircase on every level
	connectedRooms[rand()% (connectedRooms.size() - 1)].placeInRoom('<', levelMap);
	//put a down staircase on every level
	connectedRooms[rand()% (connectedRooms.size() - 1)].placeInRoom('>', levelMap);
	
	//for every room
	for (int i = 0; i < connectedRooms.size(); i++){
		//25% chance of the room having a goblin in it
		if (rand()%3 + 1 == 4)
				connectedRooms[i].placeInRoom('g', levelMap);
		//one in seven chance of a potion being put in the room.
		if (rand()%7 + 1 == 7)
			connectedRooms[i].placeInRoom('%', levelMap);
	}
			
	//return the completed string
	return levelMapToString(levelMap);
}

int main(){
	generateLevel(36, 36, true);
}