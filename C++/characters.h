//File name: characters.h
//Author: Klemente Gilbert-Espada										
//Description: This file contains the class definitions for the living
//entities inside the dungeon. The Player class and Monster classes are 
//defined here as subclasses of a parent "Character" class
//When more monsters are added to the game, they will be added here
//as classes that inherit from the "Monster" class

//the Character class describes any living entity in the dungeon.
//(for now that only means monsters and the player)
struct Character{
	//Max Hp
	int maxHp;
	//Current Hp
	int currentHp;
	//How close the character is to regaining a hit point.
	int regenCounter;
	//Character's Name
	string name;
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
	//Experience points the character has
	int xp;

	//A pointer to the dungeon the character is in
	Dungeon* dungeon;
	//A pointer to the level the character is on
	Level* currentLevel;

	//Moves the character
	void move(Tuple);
	//Attacks another character
	void attack(Character*);
	//Kills the character
	void die(Character*);
	//Heals the character 1 hp every ten turns
	void healNaturally();
	//Returns the tile the character is standing on
	Tile* getTile(void);
	//Returns the tiles in the character's line of sight
	vector<Tile*> getLineOfSight();
	//The items the character is currently holding
	vector<Item*> inventory;
};

//The player class is the one that represents the
//actual player character in the game.
struct Player: Character{
	//Player initializer
	Player (int, int);
	//Turn handling function
	bool takeTurn(char);
};

//The monster class represents every other character
//in the dungeon.
struct Monster: Character{
//	This stack should be implemented to keep track
//of wherever the monster is currently going
//	stack <Tuple> path;

	//findPath returns a stack of directions to the
	//target tile
	stack <Tuple> findPath(Tile*);
	//Handles the monster's turn
	void takeTurn(Character);
	//Handle's the monster's death
	void die(Character*);
	//Return's the monster's id, which 
	//keeps track of the monster's position 
	//in the current level's monster vector
	int getId(void);
	//If the player is in the monster's line of sight,
	//this is true, otherwise it's false
	bool canSeePlayer;
};

//The spaceGoblin is our (only) example monster
struct SpaceGoblin: Monster{
	//Initializer
	SpaceGoblin(int, int, Level*);
};