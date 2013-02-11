#include "GameWorld.h"

#include "Game.h"

using namespace C4;

GameWorld::GameWorld(const char *name) : World(name)
{
	// This constructor is called when the Game::ConstructWorld() function is
	// called to create a new world class. The world hasn't actually been loaded
	// from disk yet when we get here.
	
	spawnLocator = nullptr;
}

GameWorld::~GameWorld()
{
}

const LocatorMarker *GameWorld::FindSpawnLocator(const Zone *zone)
{
	// Iterate through all of the markers in the zone.
	
	const Marker *marker = zone->GetFirstMarker();
	while (marker)
	{
		MarkerType type = marker->GetMarkerType();
		if (type == kMarkerLocator)
		{
			const LocatorMarker *locator = static_cast<const LocatorMarker *>(marker);
			if (locator->GetLocatorType() == kLocatorSpawn) return (locator);
		}
		
		// Get the next marker in the list. (Markers are elements of both a list
		// of nodes and a list of markers, so we need to explicitly specify which
		// one we're asking for with the ListElement<Marker>::Next() expression.)
		
		marker = marker->ListElement<Marker>::Next();
	}
	
	// Look in all of the subzones.
	
	const Zone *subzone = zone->GetFirstSubzone();
	while (subzone)
	{
		const LocatorMarker *locator = FindSpawnLocator(subzone);
		if (locator) return (locator);
		
		subzone = subzone->ListElement<Zone>::Next();
	}
	
	return (nullptr);
}

ResourceResult GameWorld::Preprocess(void)
{
	// The Preprocess() function is called after the world has been constructed.
	// We must always call the base class Preprocess() function first. If it returns
	// an error, then we just return the same result code.
	
	ResourceResult result = World::Preprocess();
	if (result != kResourceOkay) return (result);
	
	// The world is now completely loaded. We search for a locator node that
	// represents the player's spawn position. It will have a locator
	// type of kLocatorSpawn.
	
	spawnLocator = FindSpawnLocator(GetRootZone());
	
	return (kResourceOkay);
}

void GameWorld::Render(void)
{
	// This function is called once per frame to render the world.
	// The subclass may do whatever it needs to before or after rendering,
	// but at some point must call World::Render().
	
	World::Render();
}