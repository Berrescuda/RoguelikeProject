#include <iostream>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <deque>
#include <list>
#include <stack>
using namespace std;

deque <string> gameLog;

#include "headers.h"

#include "characters.c++"
Player player(0, 0);
#include "display.c++"


#include "mapObjects.c++"


int main(){
	char c;
	initCurses();
	Level levelOne = Level(rawMap);
	player.currentLevel = &levelOne;
	levelOne.processTurn();

	cursesCleanup();
}
