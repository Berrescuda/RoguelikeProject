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

struct Monster: Character{

};

struct SpaceGoblin: Monster{
	SpaceGoblin(int, int);
};

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

/*
int main(){
	//Initialize the player
	Player pc;
	
	Player monster;

	monster.name = "monster";

	pc.attack(&monster);
	//Print the player's name
	cout << monster.currentHp << endl;
	//Print the player's color
	cout << pc.color << endl;
	return 0;
}
*/
