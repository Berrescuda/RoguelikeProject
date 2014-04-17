///////////////////////////////////////////////////////////////////////////
//File name: display.c++
//Author: Klemente Gilbert-Espada										
//Description: This file contains the functions for drawing the game screen
//(the map of the level, the window containing info of the players stats)
//(UI stuff)
///////////////////////////////////////////////////////////////////////////

int drawMap(Player player){
	int levelWidth = 36;
	int levelHeight = 18;
	int i, j;
	char levelMap[18][18] = {
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},
		{'#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'},		
	};
	//We start in the center of the screen and work outwards from there.
	int y = 9;
	//We go down the map row by row
	for(i = 1; i < levelHeight; i++){
		//We initialize X here because we start at the center of the screen,
		//but for asthetic reasons, every in game x coordinate is two spaces
		//as far as curses is concerned
		int x = 9;
		//Make sure we don't collide with the edges of the map and try to draw
		//things that aren't there.
		//if (y - player.yPos > 0 && y - player.yPos < levelHeight){
			//Iterate over every square on the row
			for(j = 1; j < 18; j++){
				//Get the information about how to display that square
//char squareDisplay = levelMap[i][j].printTile()
				//Make sure we're not going to collide with either side of our window
		//		if (x - player.xPos > 0 and (x * 2) - (player.xPos * 2) < levelWidth){
					//Print the current square to the screen
					mvaddch(i, j * 2, levelMap[i][j]);
		//			mvaddch(y - player.yPos, (x * 2) - (player.xPos * 2), levelMap[i][j]);
				//}
				//Next square
				x++;
			//}
		}
	}
	//next row
	y++;
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

//our mainDisplay function is where our primary in game curses calls take place
//paramaters: none
//returns: nothing
int mainDisplay(){
	int levelHeight = 18;
	int levelWidth = 36;
	//clear the screen of any leftover residue
	clear();

	//Draw a box around the map screen
	drawBox(0, 0, levelHeight, levelWidth);

	//draw a box around our output zone
	drawBox(levelHeight + 1, 0, 4, levelWidth);

	//characters.drawLog(levelHeight + 2, 1, screen)

	//draw our stats display window:
	drawStatsWindow(0, levelWidth + 1, levelHeight, 23, "foobar");

	Player player;
	player.xPos = 9;
	player.yPos = 9;

	drawMap(player);

	//Refresh the screen so the changes will take effect
	refresh();
}
