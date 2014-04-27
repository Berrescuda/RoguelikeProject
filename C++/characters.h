struct Character;
struct Player;
struct Monster;
struct SpaceGoblin;

struct Level;

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

	int move(int*);
	int attack(Character*)
};

struct Player: Character{
	//The player has experience points
	int xp;

	Player (int, int);

	void takeTurn(char c){
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
			move(direction);
			passTurn = true;
		}
	}
};
