/////////////////////////////////////////////////////////////////////////////
//File name: characters.cpp
//Author: Klemente Gilbert-Espada										
//Description: This file contains the class definitions for the living
//entities inside the dungeon. The Player class and Monster classes are 
//defined here as subclasses of a parent "Character" class
//When more monsters are added to the game, they will be added here
//as classes that inherit from the "Monster" class
////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <ncurses.h>
using namespace std;


//the Character class describes any living entity in the dungeon.
//(for now that only means monsters and the player)
class Character{
	public:
		//Max Hp
		int maxHp;
		//Current Hp
		int currentHp;
		//Character's Name
		const char* name;
		//Character Symbol
		char symbol;
		//Color the Symbol will display
		int color;
		//A quick flag to determine if the character is our PC
		bool isPlayer;
		//The character's position on the x axis
		int xPos;
		//The character's position on the y axis
		int yPos;
		//The amount of damage the character deals in combat
		int power;
}character;

class Player: public Character{
	//The player has experience points
	int xp;

	public:
		Player (void);
}player;

Player::Player (void){
	//Set up our player initializer
	maxHp = 10;
	currentHp = maxHp;
	symbol = '@';
	name = "foobar";
	color = COLOR_RED;
	isPlayer = true;
	power = 1;
}

/*int main(){
	//Initialize the player
	Player pc;
	//Print the player's symbol
	cout << pc.symbol << endl;
	//Print the player's name
	cout << pc.name << endl;
	//Print the player's color
	cout << pc.color << endl;
	return 0;
}*/