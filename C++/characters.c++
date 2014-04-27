/////////////////////////////////////////////////////////////////////////////
//File name: characters.cpp
//Author: Klemente Gilbert-Espada										
//Description: This file contains the class definitions for the living
//entities inside the dungeon. The Player class and Monster classes are 
//defined here as subclasses of a parent "Character" class
//When more monsters are added to the game, they will be added here
//as classes that inherit from the "Monster" class
////////////////////////////////////////////////////////////////////////////
using namespace std;

int Character::move(int y, int x){
	Tile targetTile = currentLevel->levelMap[yPos + y][xPos + x];
	if(targetTile.terrain.passable && targetTile.character.symbol == ' '){
		currentLevel->levelMap[yPos][xPos].character = NullCharacter();
		yPos += y;
		xPos += x;
		//const Character ourGuy = *this;
		currentLevel->levelMap[yPos][xPos].character = *this;
	}
}

NullCharacter::NullCharacter(void){
	symbol = ' ';
	name = "NULL";
}

Player::Player (int y, int x){
	//Set up our player initializer
	maxHp = 10;
	currentHp = maxHp;
	symbol = '@';
	name = "foobar";
	color = COLOR_RED;
	isPlayer = true;
	power = 1;
	xPos = x;
	yPos = y;
}

void Player::takeTurn(char c){
		bool passTurn = false;

		int direction[2] = {0, 0};

		switch(c){
			case 56:
			case 'w':
				direction[0] = -1;
				break;

			case 50:
			case 's':
				direction[0] = 1;
				break;

			case 52:
			case 'a':
				direction[1] = -1;
				break;

			case 54:
			case 'd':
				direction[1] = 1;
				break;

			case 49:
				direction[0] = 1;
				direction[1] = -1;
				break;
			case 51:
				direction[0] = 1;
				direction[1] = 1;
				break;

			case 55:
				direction[0] = -1;
				direction[1] = -1;
				break;

			case 57:
				direction[0] = -1;
				direction[1] = 1;
				break;

			case 53:
				passTurn = true;
		}
		if(direction[0] != 0 || direction[1] != 0){
			move(direction[0], direction[1]);
			passTurn = true;
		}
	}


SpaceGoblin::SpaceGoblin(int y, int x){
	xPos = x;
	yPos = y;
	maxHp = 5;
	currentHp = maxHp;
	symbol = 'g';
	name = "Space Goblin";
	color = COLOR_RED;
	isPlayer = false;
	power = 1;
}
