#include "Game.h"

// Include Registrars
#include "ActionRegistrar.h"
#include "CamControlRegistrar.h"
#include "DevRegistrar.h"
#include "TrapRegistrar.h"
#include "PropertyRegistrar.h"

using namespace C4;

// instance of the Game
Game *C4::TheGame = nullptr;

C4::Application *ConstructApplication(void)
{
	// Initialize Registrars
	new ActionRegistrar;
	new CamControlRegistrar;
	new DevRegistrar;
	new TrapRegistrar;
	new PropertyRegistrar;

	return (new Game);
}


Game::Game() : Singleton<Game>(TheGame),
	displayEventHandler(&HandleDisplayEvent),
	playerEntityReg(kEntityPlayer, "Char", kEntityPrecache, kControllerPlayer),
	locatorReg(kLocatorSpawn, "Spawn Location")
{
	// This installs an event handler for display events. This is only
	// necessary if we need to perform some action in response to
	// display events for some reason.
	
	TheDisplayMgr->InstallDisplayEventHandler(&displayEventHandler);
	
	// The following lines set the radius, height, depth, and color that the
	// player entity has when placed in the World Editor.
	
	playerEntityReg.SetEntitySize(0.35F, 2.0F, 0.0F);
	playerEntityReg.SetEntityColor(ColorRGB(1.0F, 0.0F, 0.0F));
	
	// This sets the function that is called when the user hits the
	// escape key during gameplay. We save the old function so that
	// it can be restored when the game DLL is unloaded.
	
	prevEscapeProc = TheInputMgr->GetEscapeProc();
	prevEscapeData = TheInputMgr->GetEscapeData();
	TheInputMgr->SetEscapeProc(&EscapeProc, this);
	
	// This registers our world class constructor with the World Manager.
	// We only need to do this if we have defined a subclass of the World
	// class that holds extra information.
	
	TheWorldMgr->SetWorldConstructor(&ConstructWorld);
			
	playerController = nullptr;
	
	//used to flag if a new world should be loaded
	loadWorld = false; 
	//used to show the loading text before a world loads
	showText = true;

	

	// initialize all buttons (actions)
	TheActReg->LoadActions();

	// initialize camera commands and scripts
	TheCamCtrlReg->LoadCommands();
	TheCamCtrlReg->LoadScripts();

	// initialize all development commands
	TheDevReg->LoadCommands();

	// initialize traps
	TheTrapReg->RegisterTraps();
	
	//initialize properties
	ThePropReg->RegisterProperties();

	camTriggers = true;
	gridMove	= false;

	playerMonkeys[0] = false;
	playerMonkeys[1] = false;
	playerMonkeys[2] = false;
	playerCoinAmount = 0;



	MainWindow::Open();
}

Game::~Game() {
	// When the game DLL is about to be unloaded, this destructor is called.
	TheWorldMgr->UnloadWorld();
	TheWorldMgr->SetWorldConstructor(nullptr);
		
	// Restore the previous escape key handling function.
	TheInputMgr->SetEscapeProc(prevEscapeProc, prevEscapeData);

	delete TheActReg;
	delete TheCamCtrlReg;
	delete TheDevReg;
	delete TheTrapReg;
	delete ThePropReg;
}

World *Game::ConstructWorld(const char *name, void *data)
{
	// This function is called when a new world is being loaded. It should
	// return a pointer to a newly constructed subclass of the World class.
	
	return (new GameWorld(name));
}

void Game::HandleDisplayEvent(EventType eventType, long param, void *data)
{
	// This function is called when a display event occurs (because we
	// registered it in the Game constructor).
	
	if (eventType == kEventDisplayChanged)
	{
		// The screen resolution has changed. Handle accordingly.
	}
}

void Game::EscapeProc(void *data)
{
}

EngineResult Game::LoadWorld(const char *name) {
	// Attempt to load the world.
	WorldResult result = TheWorldMgr->LoadWorld(name);
	if (result == kWorldOkay)
	{
		GameWorld *world = static_cast<GameWorld *>(TheWorldMgr->GetWorld());
		const LocatorMarker *locator = world->GetSpawnLocator();
		if (locator)
		{
			// If a spawn locator was found in the world, put a player character there.
			
			// The BeginSinglePlayerGame() sets of some messaging functionality that is
			// needed by the CharacterController class.
			
			TheMessageMgr->BeginSinglePlayerGame();
			
			// Load a player model and attach a controller to it.
			
			Entity *entity = Entity::Get(kEntityPlayer);
			PlayerController *controller = new PlayerController(locator->GetWorldPosition());
			entity->SetController(controller);
			TheGame->playerController = controller;
			
			// Put the entity in the world inside the locator's zone.
			
			locator->GetOwningZone()->AddNewSubnode(entity);
			controller->SetLastCheckpoint(locator->GetNodePosition());

			// Set the world's current camera to be our chase camera.
			// The world will not render without a camera being set.
			
			SideCamera *camera = world->GetSideCamera();
			camera->SetTargetEntity(entity);
			camera->ResetHeight();
			camera->SetOffset(Vector2D(1.f,2.f));
			camera->SetDist(8.f);
			camera->SetRotation(0.f,0.f);
			camera->SetTrackType(kCamLock);
			camera->SetTrackAmount(0.5f);
			camera->SetFlipFlags(3);

			world->SetCamera(camera);
			DisplayInterface::Open();
		}
	}

	return (result);
}

WorldResult Game::StartSinglePlayerGame(const char *name)
{
	delete TheMainWindow;
	ExitCurrentGame(false);
	
	WorldResult result = TheWorldMgr->LoadWorld(name);
	if (result == kWorldOkay)
	{
		GameWorld *world = static_cast<GameWorld *>(TheWorldMgr->GetWorld());
		const LocatorMarker *locator = world->GetSpawnLocator();
		if (locator)
		{
			// If a spawn locator was found in the world, put a player character there.
			
			// The BeginSinglePlayerGame() sets of some messaging functionality that is
			// needed by the CharacterController class.
			
			TheMessageMgr->BeginSinglePlayerGame();
			
			// Load a player model and attach a controller to it.
			
			Entity *entity = Entity::Get(kEntityPlayer);
			PlayerController *controller = new PlayerController(locator->GetWorldPosition());
			entity->SetController(controller);
			TheGame->playerController = controller;
			
			// Put the entity in the world inside the locator's zone.
			
			locator->GetOwningZone()->AddNewSubnode(entity);
			controller->SetLastCheckpoint(locator->GetNodePosition());

			// Set the world's current camera to be our chase camera.
			// The world will not render without a camera being set.
			
			SideCamera *camera = world->GetSideCamera();
			camera->SetTargetEntity(entity);
			camera->ResetHeight();
			camera->SetOffset(Vector2D(1.f,2.f));
			camera->SetDist(8.f);
			camera->SetRotation(0.f,0.f);
			camera->SetTrackType(kCamLock);
			camera->SetTrackAmount(0.5f);
			camera->SetFlipFlags(3);

			world->SetCamera(camera);
			DisplayInterface::Open();
		}
	}
	
	return (result);
}

void Game::ExitCurrentGame(bool unload)
{
	TheMessageMgr->EndGame();

	
	if (unload) TheWorldMgr->UnloadWorld();
	
	delete TheDisplayInterface;
}

void Game::UnloadWorld(void) {
	TheWorldMgr->UnloadWorld();
	TheMessageMgr->EndGame();
	TheGame->playerController = nullptr;
}

void Game::ApplicationTask(){
	Application::ApplicationTask();

	//if(offsetPlayer) {
		//offsetPlayer = false;
		//Point3D pos = playerController->GetTargetNode()->GetWorldPosition() + offsetAmt;
	//}

	//should we load the next world
	//and has the has the text already been shown?
	if(loadWorld && !showText){	
		//load the world
		LoadWorld(nextWorld);
		loadWorld = false;
	//	showText = true;
	}

	//Should we load the next world?
	//Do we also need to show the text?
	if(loadWorld && showText){
	//change the text to say loading
	//	loadingText->SetText("Loading...");

		//before we load the new level, save the information about the players current coins and monkey parts
		playerCoinAmount = TheGame->GetPlayerController()->GetCoin();//get current gold amount

		for(int i = 0; i<3; i++){
			//does the player have current monkey piece?
			if(TheGame->GetPlayerController()->GetMonkey(i)){
				playerMonkeys[i] = true;//if so log it
			}
		}

		//set up the text for Loading levels using a custom font
	loadingText = new TextElement("Loading...", AutoReleaseFont("font/papyrus"));

		
	loadingText->SetElementPosition(Point3D(400.0f, 300.0f, 0.0f));
	loadingText->SetTextColor(ColorRGBA(1.0F, 1.0F, 0.1F, 1.0F));
	TheInterfaceMgr->AddElement(loadingText);

		//we set up the text to be shown so set it to false
		showText = false;
	}

	//If we already loaded the world
	//and already showed the text
	if((loadWorld == false) && (showText == false)){
		//reset showtext so we know to show it next time
		showText = true;
		//hide the loading text until next time
		loadingText->SetText("");

		TheGame->GetPlayerController()->AddCoin(playerCoinAmount);//set current gold amount
		for(int i = 0; i<3; i++){
			if(playerMonkeys[i])//did player have given monkey piece?
				TheGame->GetPlayerController()->AddMonkey(i);//if so re-add it
		}
		
		//after we update the players collected info
		//re-update all the interfaces so i looks acurate
		DisplayInterface::UpdateAll();
	}
}