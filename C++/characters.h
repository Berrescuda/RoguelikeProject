//the Character class describes any living entity in the dungeon.
//(for now that only means monsters and the player)
struct Character{
	//Max Hp
	int maxHp;
	//Current Hp
	int currentHp;
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

	Level* currentLevel;

	int move(int, int);
	int attack(Character*);
};

struct Player: Character{
	//The player has experience points
	int xp;

	Player (int, int);

	void takeTurn(char);
};

struct Monster: Character{

};

struct SpaceGoblin: Monster{
	SpaceGoblin(int, int);
};