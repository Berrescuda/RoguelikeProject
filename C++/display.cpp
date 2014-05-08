///////////////////////////////////////////////////////////////////////////
//File name: display.cpp
//Author: Klemente Gilbert-Espada										
//Description: This file contains the functions for drawing the game screen
//(the map of the level, the window containing info of the players stats)
//(UI stuff)
///////////////////////////////////////////////////////////////////////////

int drawMap(Level level){
	int levelWidth = 36;
	int levelHeight = 18;
	int i, j;
	vector<vector<Tile> > levelMap = level.levelMap;
	//We go down the map row by row
	for(i = player.yPos - 8; i <= player.yPos + 8; i++){
		//make sure the row we're about to print exists
		if (0 < i && i < levelMap.size()){
			for(j = player.xPos - 8; j <= player.xPos + 8; j++){
				//Make sure we're not going to collide with either side of our window
				if (0 < j && j < levelMap[i].size()){
					if(levelMap[i][j].explored){
						//Get the information about how to display that square
						TileDisplayData squareDisplay = levelMap[i][j].printTile();
						//Print the current square to the screen
						//...if we can see it
						if(!levelMap[i][j].visible)
							//otherwise we print it in magenta
							squareDisplay.color = COLOR_MAGENTA;
						//color on
						attron(COLOR_PAIR(squareDisplay.color));
						//print character
						mvaddch(i - player.yPos + 9, (j - player.xPos) * 2 + 18, squareDisplay.symbol);
						//color off
						attroff(COLOR_PAIR(squareDisplay.color));
					}
				}
			}
		}
	}
}
/*
*/

/*
Our statsWindow function displays basic character stats in a box of the size and at
the location of the player's choice.
paramaters: y pos, x pos, height of window, width of window, screen to draw on
returns nothing
*/
int drawStatsWindow(int y, int x, int height, int width, const char* name){
	//first we draw the window itself
	drawBox(y, x, height, width);
	x++;
	//print the player name at the top of the window
	mvaddstr(y + 1, x, name);
	//display player hp
	mvaddstr(y + 2, x, "hp:");

	//Figure out how long the health bar is
	int healthBarLength = ((10 * player.currentHp)/player.maxHp);
	//hp bar
	hColoredLine(y + 2, x + 3, '=', healthBarLength, COLOR_GREEN);
	//display current hp/max hp
	mvaddstr(y + 2, x + 14, (to_string(player.currentHp) + '/' + to_string(player.maxHp)).c_str());
	
	//print player XP
	mvaddstr(y + 4, x, "XP:");
	string xp = to_string(player.xp);
	mvaddstr(y + 4, x + 4, xp.c_str());

	//Print the current level of the dungeon the player is on
	mvaddstr(y + 5, x, "Current Level: ");
	mvaddstr(y + 5, x + 15, to_string(player.currentLevel->levelNumber).c_str());

	//Print the items in the character's inventory.
	mvaddstr(y + 6, x, "Inventory:");

	if (!player.inventory.empty())
		for(int i = 0; i < player.inventory.size(); i++)
			mvaddstr(y + 7 + i, x, player.inventory[i]->name.c_str());
}

void drawLogWindow(int y, int x, int height, int width){
	//draw a box around our display zone
	drawBox(y, x, height, width);
	for(int i = 0; i < 3 ; i++){
		mvaddstr(y + 3 - i, x + 1, gameLog[i].c_str());
	}
}

//This is just a wrapper that takes a string and
//passes it along to the log
void log(string message){
	gameLog.push_front(message);
}

//our mainDisplay function is where our primary in game curses calls take place
//paramaters: none
//returns: nothing
int mainDisplay(Level level){
	int boxHeight = 18;
	int boxWidth = 36;
	//clear the screen of any leftover residue
	clear();

	//Draw a box around the map screen
	drawBox(0, 0, boxHeight, boxWidth);

	drawLogWindow(boxHeight + 1, 0, 4, boxWidth);

	//characters.drawLog(levelHeight + 2, 1, screen)

	//draw our stats display window:
	drawStatsWindow(0, boxWidth + 1, boxHeight, 23, "foobar");

	drawMap(level);

	//Refresh the screen so the changes will take effect
	refresh();
}
