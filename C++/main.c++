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


#include "mapObjects.c++"
#include "kcurses.c++"
#include "display.c++"


int main(){
	char c;
	initCurses();
	Level levelOne = Level(rawMap);
	player.currentLevel = &levelOne;
	while(c != 'q'){
	mainDisplay(levelOne);
	c = getch();
	player.takeTurn(c);
	}

	cursesCleanup();
}
