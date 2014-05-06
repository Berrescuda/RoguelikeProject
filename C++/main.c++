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
	Level levelOne = Level(generateLevel(36, 36, true));
	player.currentLevel = &levelOne;
	player.lineOfSight = player.getLineOfSight();
	levelOne.processTurn();

	cursesCleanup();
}
