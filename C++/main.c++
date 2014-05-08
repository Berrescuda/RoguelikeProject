//File Name: main.c++
//Author: Klemente Gilbert-Espada
//Description: The file that runs the game

//Include libraries we didn't write
#include <iostream>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <deque>
#include <list>
#include <stack>
#include <time.h>
#include <string>

using namespace std;

//Declare a global log
deque <string> gameLog;

//import all of our header files
#include "headers.h"

//Import the level generator
#include "levelGenerator.c++"
//The character functions
#include "characters.c++"
//globally declare the player (this is lazy, will fix later)
Player player(0, 0);
//The visual component
#include "display.c++"
//How tiles behave
#include "mapObjects.c++"
//How items behave
#include "items.cpp"

//Our main function sets up what it needs to set up, 
//it initializes curses, the dungeon, and runs the primary
//loop that defines the game, and then quits when we're done
int main(){
	//Declare our input character
	char c;
	//Set up curses
	initCurses();
	//Initialize the dungeon
	Dungeon dungeon;

	//Initialize five levels of the dungeon
	for(int i = 0; i < 5; i++){
		//Declare a new level
		Level* newLevel = new Level(generateLevel(36, 36, i));
		//Tell it what level it is
		newLevel->levelNumber = i;
		//Push it onto the dungeon's vector of levels
		dungeon.level.push_back(newLevel);
	}
	
	//The player starts on the first level
	player.currentLevel = dungeon.level[0];
	//Point the player to the dungeon
	player.dungeon = &dungeon;
	//Figure out what the player can see
	player.getLineOfSight();
	//Start the game
	dungeon.processTurn();

	cursesCleanup();
}
