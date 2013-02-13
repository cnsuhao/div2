#include "C4FilePicker.h"
#include "Game.h"
#include "Interface.h"


using namespace C4;


namespace
{


enum
	{
		kServerAliveCount		= 10,
		kServerQueryInterval	= 1000,
		
		kChatHistoryCount		= 24,
		kMessageLineCount		= 6,
		kMessageScrollTime		= 3000
	};
	
	
	enum
	{
		kWorldListLeft		= 16,
		kWorldListTop		= 29,
		kWorldListRight		= kWorldListLeft + 168,
		kWorldListBottom	= kWorldListTop + 104,
	};
	
	
	const ColorRGBA kWindowBackgroundColor(0.0F, 0.0F, 0.0F, 0.375F);
}






DisplayInterface *C4::TheDisplayInterface = nullptr;
SinglePlayerWindow *C4::TheSinglePlayerWindow = nullptr;
MainWindow *C4::TheMainWindow = nullptr;



TextButtonElement::TextButtonElement(const char *text, Font *font) : TextElement(text, font)
{
	textColor = K::gray_50;
	hiliteColor = K::white;
}

TextButtonElement::~TextButtonElement()
{
}

void TextButtonElement::SetColors(const ColorRGBA& text, const ColorRGBA& hilite)
{
	textColor = text;
	hiliteColor = hilite;
}

bool TextButtonElement::CalculatePickBox(Box2D *box) const
{
	const Font *font = GetFont();
	float h = (font) ? font->GetFontHeader()->fontHeight : 0.0F;
	float w = GetFormattedTextWidth();
	
	switch (GetTextAlignment())
	{
		case kTextAlignLeft:
			
			box->upperLeft.Set(0.0F, 0.0F);
			box->lowerRight.Set(w, h);
			break;
		
		case kTextAlignCenter:
			
			box->upperLeft.Set(w * -0.5F, 0.0F);
			box->lowerRight.Set(w * 0.5F, h);
			break;
		
		case kTextAlignRight:
			
			box->upperLeft.Set(-w, 0.0F);
			box->lowerRight.Set(0.0F, h);
			break;
	}
	
	return (true);
}

PartCode TextButtonElement::TestPickPoint(const Point3D& p) const
{
	return (kElementPartButton);
}

void TextButtonElement::HandleMouseEvent(EventType eventType, PartCode code, const Point3D& p)
{
	bool click = false;
	unsigned long state = GetElementState();
	
	if (eventType == kEventMouseDown)
	{
		state |= kElementHilited;
	}
	else
	{
		const Font *font = GetFont();
		float h = (font) ? font->GetFontHeader()->fontHeight : 0.0F;
		float w = GetFormattedTextWidth();
		
		bool inside = ((p.y >= 0.0F) && (p.y <= h));
		if (inside)
		{
			switch (GetTextAlignment())
			{
				case kTextAlignLeft:
					
					inside &= ((p.x >= 0.0F) && (p.x <= w));
					break;
				
				case kTextAlignCenter:
					
					inside &= ((p.x >= w * -0.5F) && (p.x <= w * 0.5F));
					break;
				
				case kTextAlignRight:
					
					inside &= ((p.x >= -w) && (p.x <= 0.0F));
					break;
			}
		}
		
		if (eventType == kEventMouseMoved)
		{
			if (inside) state |= kElementHilited;
			else state &= ~kElementHilited;
		}
		else if (eventType == kEventMouseUp)
		{
			if (inside) click = true;
			state &= ~kElementHilited;
		}
	}
	
	SetElementState(state);
	if (click) TriggerElement();
}

void TextButtonElement::Build(void)
{
	unsigned long state = GetElementState();
	
	if (state & kElementHilited) SetTextColor(hiliteColor);
	else SetTextColor(textColor);
	
	TextElement::Build();
}


GameInterface::GameInterface()
{
}

GameInterface::~GameInterface()
{
}


GameWindow::GameWindow(float width, float height, const char *title, unsigned long flags) : Window(width, height, title, flags)
{
}

GameWindow::~GameWindow()
{
}




		/**
		 * Updates the amount of coins the player collected
		 */
		void DisplayInterface::UpdatePlayerCoin(void)
		{
			const PlayerController *player = TheGame->GetPlayerController();
			if (player)
			{
				coinValueText.SetText(Text::IntegerToString(player->GetCoin()));
			}
		}

		/**
		 * Updates the player's monkey collection display
		 */
		void DisplayInterface::UpdatePlayerMonkey(void)
		{
			int monkeyPieces = 0;
			const PlayerController *player = TheGame->GetPlayerController();
			if (player)
			{
				TheEngine->Report("in player Controller");

				if(player->GetMonkey(0)){//if we have piece 0(head)
					monkeyHeadImage.Show();// Activate();
					monkeyPieces++;//make that image visable
				}else
					monkeyHeadImage.Hide();

				if(player->GetMonkey(1)){//if we have piece 1(body)
					monkeyBodyImage.Show();
					monkeyPieces++;
				}else 
					monkeyBodyImage.Hide();

				if(player->GetMonkey(2)){//if we have piece 2(feet)
					monkeyFeetImage.Show();
					monkeyPieces++;
				}else
					monkeyFeetImage.Hide();

			}

			monkeyValueText.SetText(String<1>(monkeyPieces));
		}

		/**
		 * Updates all display interface functions. Used after loading a new level
		 */
		void DisplayInterface::UpdateAll(void)
		{
			TheDisplayInterface->UpdatePlayerCoin();
			TheDisplayInterface->UpdatePlayerMonkey();
			
		}

DisplayInterface::DisplayInterface() :
		Singleton<DisplayInterface>(TheDisplayInterface),
		coinImage(64.0F, 64.0F, "tga/UI-Coin"),
		monkeyEmptyImage(64.0F, 128.0F, "tga/monkey-nopieces"),
		monkeyHeadImage(64.0F, 128.0F, "monkey-head"),
		monkeyBodyImage(64.0F, 128.0F, "monkey-body"),
		monkeyFeetImage(64.0F, 128.0F, "monkey-feet")
{
		
	coinImage.SetElementPosition(Point3D(0.0F, 0.0F, 0.0F));//Point3D(296.0F, 24.0F, 0.0F));
	AddSubnode(&coinImage);
	
	monkeyEmptyImage.SetElementPosition(Point3D(TheDisplayMgr->GetDisplayWidth() - 64.0f, -64.0F, 0.0F));
	//monkeyEmptyImage.SetBlendState(BlendState(kBlendOne, kBlendInvSourceAlpha));
	AddSubnode(&monkeyEmptyImage);
	

	
	//*these parts of the monkey wont be visable until collected
	monkeyHeadImage.SetElementPosition(Point3D(TheDisplayMgr->GetDisplayWidth() - 64.0f, -64.0F, 0.0F));
	AddSubnode(&monkeyHeadImage);
	monkeyHeadImage.Hide();

	monkeyBodyImage.SetElementPosition(Point3D(TheDisplayMgr->GetDisplayWidth() - 64.0f, -64.0F, 0.0F));
	AddSubnode(&monkeyBodyImage);
	monkeyBodyImage.Hide();

	monkeyFeetImage.SetElementPosition(Point3D(TheDisplayMgr->GetDisplayWidth() - 64.0f, -64.0F, 0.0F));
	AddSubnode(&monkeyFeetImage);
	monkeyFeetImage.Hide();
	
	
	AutoReleaseFont hammer("font/Hammer");
	
	coinValueText.SetFont(hammer);
	coinValueText.SetTextColor(ColorRGBA(1.0F, 1.0F, 0.1F, 1.0F));
	coinValueText.SetTextAlignment(kTextAlignLeft);
	coinValueText.SetElementPosition(Point3D(80.0F, 24.0F, 0.0F));
	coinValueText.SetText(String<2>(TheGame->GetPlayerController()->GetCoin()));
	AddSubnode(&coinValueText);

	monkeyText.SetFont(hammer);
	monkeyText.SetTextColor(ColorRGBA(1.0F, 1.0F, 0.1F, 1.0F));
	monkeyText.SetTextAlignment(kTextAlignRight);
	monkeyText.SetElementPosition(Point3D(TheDisplayMgr->GetDisplayWidth() - 64.0f, 24.0F, 0.0F));
	monkeyText.SetText(" /3 Monkey Pieces");
	AddSubnode(&monkeyText);

	monkeyValueText.SetFont(hammer);
	monkeyValueText.SetTextColor(ColorRGBA(1.0F, 1.0F, 0.1F, 1.0F));
	monkeyValueText.SetTextAlignment(kTextAlignCenter);
	monkeyValueText.SetElementPosition(Point3D(TheDisplayMgr->GetDisplayWidth() - 369.0f, 24.0F, 0.0F));
	monkeyValueText.SetText(String<1>(0));
	AddSubnode(&monkeyValueText);
	
}

DisplayInterface::~DisplayInterface()
{
}

void DisplayInterface::Open(void)
{
	if (!TheDisplayInterface)
	{
		DisplayInterface *interface = new DisplayInterface;
		interface->UpdateDisplayPosition();
		TheGame->AddInterface(interface);
	}
}

void DisplayInterface::UpdateDisplayPosition(void)
{
	SetElementPosition(Point3D(4.0F, (float) (TheDisplayMgr->GetDisplayHeight() - 68), 0.0F));
}





StartWindow::StartWindow() : GameWindow(544.0F, 342.0F, nullptr, kWindowTitleBar | kWindowCenter)
{

	
	float width = GetWindowWidth();
	float height = GetWindowHeight();
	
	cancelButton = new PushButtonElement("Cancel", AutoReleaseFont("font/Heading"));
	cancelButton->SetElementPosition(Point3D(width - 108.0F, height - 28.0F, 0.0F));
	AddSubnode(cancelButton);
	
	AutoReleaseFont font("font/Gui");
	
	TextElement *text = new TextElement("World", font);
	text->SetElementPosition(Point3D((float) kWorldListLeft, (float) kWorldListTop - 19.0F, 0.0F));
	AddSubnode(text);
	
	BorderElement *border = new BorderElement((float) (kWorldListRight - kWorldListLeft), (float) (kWorldListBottom - kWorldListTop));
	border->SetElementPosition(Point3D((float) kWorldListLeft, (float) kWorldListTop, 0.0F));
	AddSubnode(border);
	
	worldListBox = new ListBoxElement((float) (kWorldListRight - kWorldListLeft), (float) (kWorldListBottom - kWorldListTop), 8, 13.0F, font);
	worldListBox->SetElementPosition(Point3D((float) kWorldListLeft, (float) kWorldListTop, 0.0F));
	AddSubnode(worldListBox);
	SetFocusElement(worldListBox);
	
	border = new BorderElement(320.0F, 240.0F, kBorderOutsideOnly);
	border->SetElementPosition(Point3D(212.0F, 12.0F, 0.0F));
	AddSubnode(border);
	
	previewImage = new ImageElement(320.0F, 240.0F, "tga/UI-Coin");
	previewImage->SetElementPosition(Point3D(212.0F, 12.0F, 0.0F));
	previewImage->SetAmbientLightPointer(nullptr);
	AddSubnode(previewImage);
	
	worldTable = new StringTable("text/Worlds");
	const StringHeader *stringHeader = worldTable->GetRootStringHeader();
	while (stringHeader)
	{
		const char *name = stringHeader->GetString();
		worldListBox->AddListItem(new ListBoxItem(&name[Text::GetDirectoryPathLength(name)]));
		stringHeader = stringHeader->GetNextString();
	}
	
	worldSelection = -1;
	worldListBox->SetSelection(0);
	UpdatePreview();
}

StartWindow::~StartWindow()
{
	delete worldTable;
}

const char *StartWindow::GetSelectedWorldName(void) const
{
	long selection = worldListBox->GetSelection();
	if (selection != -1)
	{
		const StringHeader *stringHeader = worldTable->GetRootStringHeader();
		while (stringHeader)
		{
			if (selection == 0) return (stringHeader->GetString());
			
			selection--;
			stringHeader = stringHeader->GetNextString();
		}
	}
	
	return (nullptr);
}

void StartWindow::UpdatePreview(void)
{
	long selection = worldListBox->GetSelection();
	if (selection != worldSelection)
	{
		worldSelection = selection;
		previewImage->SetTexture(GetSelectedWorldName());
	}
}

bool StartWindow::HandleKeyboardEvent(EventType eventType, long key, unsigned long modifiers)
{
	if (eventType == kEventKeyDown)
	{
		if (key == kKeyCodeEscape)
		{
			cancelButton->TriggerElement();
			return (true);
		}
		
		bool b = GameWindow::HandleKeyboardEvent(eventType, key, modifiers);
		if (b) UpdatePreview();
		return (b);
	}
	
	return (GameWindow::HandleKeyboardEvent(eventType, key, modifiers));
}

void StartWindow::HandleElementTrigger(Element *element)
{
	if (element == cancelButton)
	{
		CallCompletionProc();
		delete this;
	}
	else if (element == worldListBox)
	{
		UpdatePreview();
	}
}


SinglePlayerWindow::SinglePlayerWindow() : Singleton<SinglePlayerWindow>(TheSinglePlayerWindow)
{

	SetWindowTitle("Start New Game");
	
	float width = GetWindowWidth();
	float height = GetWindowHeight();
	
	startButton = new PushButtonElement("Start", AutoReleaseFont("font/Heading"));
	startButton->SetElementPosition(Point3D(width - 38.0F, height - 28.0F, 0.0F));
	startButton->SetElementState(kElementDefault);
	AddSubnode(startButton);
}

SinglePlayerWindow::~SinglePlayerWindow()
{
}

void SinglePlayerWindow::Open(void)
{
	if (TheSinglePlayerWindow) TheInterfaceMgr->SetActiveWindow(TheSinglePlayerWindow);
	else TheGame->AddWindow(new SinglePlayerWindow);
}

void SinglePlayerWindow::InterfaceTask(void)
{
	if (worldListBox->GetSelection() >= 0) startButton->Enable();
	else startButton->Disable();
	
	StartWindow::InterfaceTask();
}

bool SinglePlayerWindow::HandleKeyboardEvent(EventType eventType, long key, unsigned long modifiers)
{
	if (eventType == kEventKeyDown)
	{
		if (key == kKeyCodeReturn)
		{
			startButton->TriggerElement();
			return (true);
		}
	}
	
	return (StartWindow::HandleKeyboardEvent(eventType, key, modifiers));
}

void SinglePlayerWindow::HandleElementTrigger(Element *element)
{
	if (element == startButton)
	{
		ResourceName name(GetSelectedWorldName());
		delete this;
		
		TheGame->StartSinglePlayerGame(name);
	}
	else
	{
		StartWindow::HandleElementTrigger(element);
	}
}



MainWindow::MainWindow() :
		GameWindow(464.0F, 438.0F, nullptr, kWindowCenter | kWindowNoBackground | kWindowNoFrame),
		Singleton<MainWindow>(TheMainWindow)
{
	static const ColorRGBA gradientTop(0.5F, 0.0F, 0.0F, 1.0F);
	static const ColorRGBA gradientBottom(0.5F, 0.0F, 0.0F, 0.5F);
	static const ColorRGBA gradientLeft(0.0F, 0.0F, 0.0F, 0.5F);
	static const ColorRGBA gradientRight(0.0F, 0.0F, 0.0F, 0.25F);
	
	static const ColorRGBA textColor(0.875F, 0.5F, 0.0F, 1.0F);
	static const ColorRGBA hiliteColor(1.0F, 1.0F, 0.0F, 1.0F);
	
	float windowHeight = GetWindowHeight();
	
	QuadElement *quad = new QuadElement(106.0F, windowHeight);
	quad->SetVertexColor(0, gradientTop);
	quad->SetVertexColor(1, gradientBottom);
	quad->SetVertexColor(2, gradientBottom);
	quad->SetVertexColor(3, gradientTop);
	AddSubnode(quad);
	
	quad = new QuadElement(GetWindowWidth() - 106.0F, windowHeight);
	quad->SetElementPosition(Point3D(106.0F, 0.0F, 0.0F));
	quad->SetQuadColor(kWindowBackgroundColor);
	AddSubnode(quad);
	
	ImageElement *image = new ImageElement(90.0F, 320.0F, "game/C4Engine");
	image->SetElementPosition(Point3D(8.0F, windowHeight * 0.5F - 200.0F, 0.0F));
	AddSubnode(image);
	

	AutoReleaseFont font("font/Sword");
	
	quad = new QuadElement(302.0F, 25.0F);
	quad->SetElementPosition(Point3D(136.0F, 12.0F, 0.0F));
	quad->SetVertexColor(0, gradientLeft);
	quad->SetVertexColor(1, gradientLeft);
	quad->SetVertexColor(2, gradientRight);
	quad->SetVertexColor(3, gradientRight);
	AddSubnode(quad);
	
	TextElement *text = new TextElement("Single Player", font);
	text->SetTextColor(K::yellow);
	text->SetElementPosition(Point3D(140.0F, 12.0F, 0.0F));
	AddSubnode(text);
	
	startNewGameButton = new TextButtonElement("Start New Game", font);
	startNewGameButton->SetElementPosition(Point3D(152.0F, 44.0F, 0.0F));
	startNewGameButton->SetColors(textColor, hiliteColor);
	AddSubnode(startNewGameButton);
	

	saveCurrentGameButton = nullptr;
	exitCurrentGameButton = nullptr;
	
	float dy = 0.0F;
	const World *world = TheWorldMgr->GetWorld();
	bool multiplayer = TheMessageMgr->Multiplayer();
	
	if ((world) && (!multiplayer))
	{
			
		exitCurrentGameButton = new TextButtonElement("Exit Current Game", font);
		exitCurrentGameButton->SetElementPosition(Point3D(152.0F, 128.0F, 0.0F));
		exitCurrentGameButton->SetColors(textColor, hiliteColor);
		AddSubnode(exitCurrentGameButton);
		
		dy = 56.0F;
	}
	
	quad = new QuadElement(302.0F, 25.0F);
	quad->SetElementPosition(Point3D(136.0F, 104.0F + dy, 0.0F));
	quad->SetVertexColor(0, gradientLeft);
	quad->SetVertexColor(1, gradientLeft);
	quad->SetVertexColor(2, gradientRight);
	quad->SetVertexColor(3, gradientRight);
	AddSubnode(quad);
	
		
	if ((world) && (multiplayer))
	{
		exitCurrentGameButton = new TextButtonElement("Exit Current Game", font);
		exitCurrentGameButton->SetElementPosition(Point3D(152.0F, 192.0F + dy, 0.0F));
		exitCurrentGameButton->SetColors(textColor, hiliteColor);
		AddSubnode(exitCurrentGameButton);
		
		dy = 28.0F;
	}
	/*
	quad = new QuadElement(302.0F, 25.0F);
	quad->SetElementPosition(Point3D(136.0F, 196.0F + dy, 0.0F));
	quad->SetVertexColor(0, gradientLeft);
	quad->SetVertexColor(1, gradientLeft);
	quad->SetVertexColor(2, gradientRight);
	quad->SetVertexColor(3, gradientRight);
	AddSubnode(quad);
	
	quad = new QuadElement(302.0F, 25.0F);
	quad->SetElementPosition(Point3D(136.0F, 400.0F, 0.0F));
	quad->SetVertexColor(0, gradientLeft);
	quad->SetVertexColor(1, gradientLeft);
	quad->SetVertexColor(2, gradientRight);
	quad->SetVertexColor(3, gradientRight);
	AddSubnode(quad);*/
	
	quitButton = new TextButtonElement("Quit", font);
	quitButton->SetElementPosition(Point3D(434.0F, 400.0F, 0.0F));
	quitButton->SetColors(textColor, hiliteColor);
	quitButton->SetTextAlignment(kTextAlignRight);
	AddSubnode(quitButton);
}

MainWindow::~MainWindow()
{
}

void MainWindow::Open(void)
{
	if (TheMainWindow) TheInterfaceMgr->SetActiveWindow(TheMainWindow);
	else TheGame->AddWindow(new MainWindow);
}

void MainWindow::SinglePlayerComplete(StartWindow *window, void *data)
{
	Open();
}


bool MainWindow::HandleKeyboardEvent(EventType eventType, long key, unsigned long modifiers)
{
	if (eventType == kEventKeyDown)
	{
		if (key == kKeyCodeEscape)
		{
			delete this;
			return (true);
		}
	}
	
	return (GameWindow::HandleKeyboardEvent(eventType, key, modifiers));
}

void MainWindow::HandleElementTrigger(Element *element)
{
	if (element == startNewGameButton)
	{
		//SinglePlayerWindow::Open();
		//TheSinglePlayerWindow->SetCompletionProc(&SinglePlayerComplete);
		TheGame->LoadWorld("startinglevel");
		delete this;
	}
	
	else if (element == exitCurrentGameButton)
	{
		TheGame->ExitCurrentGame();
		delete this;
		Open();
	}
	else if (element == quitButton)
	{
		TheEngine->Quit();
	}
}

