#include <iostream>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <deque>
#include <list>
#include <stack>

//#include <stdio.h>
#include <time.h>
#include <string>

using namespace std;

deque <string> gameLog;

#include "headers.h"
#include "levelGenerator.c++"

#include "characters.c++"
Player player(0, 0);
#include "display.c++"


#include "mapObjects.c++"


int main(){
	char c;
	initCurses();
	Dungeon dungeon;

	
	Level levelOne = Level(rawMap);
	levelOne.levelNumber = 0;
	dungeon.level.push_back(&levelOne);

	Level levelTwo = Level(generateLevel(36, 36, false));
	levelTwo.levelNumber = 1;
	dungeon.level.push_back(&levelTwo);
	
	player.currentLevel = &levelOne;
	player.dungeon = &dungeon;
	player.lineOfSight = player.getLineOfSight();
	dungeon.processTurn();

	cursesCleanup();
}
