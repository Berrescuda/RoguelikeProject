#include <iostream>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <deque>
#include <list>
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
	for(int i = 0; i < gameLog.size(); i++)
		cout << gameLog[i] << endl;
	//cout << levelOne.levelMap[10][10].character.currentLevel->level << endl;
}
