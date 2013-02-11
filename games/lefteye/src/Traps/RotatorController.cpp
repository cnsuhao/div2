#include "RotatorController.h"

#include "C4Geometries.h"
#include "Player.h"
#include "Game.h"

using namespace C4;


/**
 * Constructs a rotator controller with a random type
 */
RotatorController::RotatorController() : Controller(kControllerRotator)
{
	
}

/**
 * Prepare the mushrom for entry into the world
 */
void RotatorController::Preprocess()
{
	Controller::Preprocess();

	// add interaction property to node
	GetTargetNode()->AddProperty(new InteractionProperty());
}

/**
 * Handle the player interacting with the Rotator
 */
void RotatorController::HandleInteractionEvent(InteractionEventType type, const Point3D *position, Node *activator)
{

	PlayerController* sc = TheGame->GetPlayerController();
	sc->SetPlayerAzimuth(sc->GetPlayerAzimuth() + K::pi_over_2);
}