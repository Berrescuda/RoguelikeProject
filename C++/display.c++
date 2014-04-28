///////////////////////////////////////////////////////////////////////////
//File name: display.c++
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
				//Get the information about how to display that square
//char squareDisplay = levelMap[i][j].printTile()
				//Make sure we're not going to collide with either side of our window
				if (0 < j && j < levelMap[i].size()){
					//Print the current square to the screen
					mvaddch(i - player.yPos + 9, (j - player.xPos) * 2 + 18, levelMap[i][j].printTile());
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
	int healthBarLength = ((10 * 10)/10);
	//hp bar
	hColoredLine(y + 2, x + 3, '=', healthBarLength, COLOR_GREEN);
	//display current hp/max hp
	mvaddstr(y + 2, x + 14, "10/10");
	
	//print player XP
	mvaddstr(y + 4, x, "XP:0");

	//Print the current level of the dungeon the player is on
	mvaddstr(y + 5, x, "Current Level: 1");

	//Print the items in the character's inventory.
	mvaddstr(y + 6, x, "Inventory:");
	/*
	for i in range(len(player.inventory)):
		mvaddstr(y + 7 + i, x, player.inventory[i].name)
	*/
}

void drawLogWindow(int y, int x, int height, int width){
	//draw a box around our display zone
	drawBox(y, x, height, width);
	for(int i = 0; i < 3 ; i++){
		mvaddstr(y + 3 - i, x + 1, gameLog[i].c_str());
	}
}


void log(string message){
	string input;
	input = message;
	gameLog.push_front(input);
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
