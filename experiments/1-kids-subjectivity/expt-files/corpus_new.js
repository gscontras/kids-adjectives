// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"yellow", "Class":"color"},
		{"Predicate":"brown", "Class":"color"},
		{"Predicate":"blue", "Class":"color"},
		{"Predicate":"golden", "Class":"color"},
		{"Predicate":"tan", "Class":"color"},
		{"Predicate":"pink", "Class":"color"},
		{"Predicate":"green", "Class":"color"},
		{"Predicate":"orange", "Class":"color"},
		{"Predicate":"silver", "Class":"color"},
		{"Predicate":"red", "Class":"color"},
		{"Predicate":"white", "Class":"color"},
		{"Predicate":"grey", "Class":"color"},
		{"Predicate":"gold", "Class":"color"},
		{"Predicate":"black", "Class":"color"},
		{"Predicate":"gray", "Class":"color"},
		{"Predicate":"tawny", "Class":"color"},
		{"Predicate":"pale", "Class":"color"},
		{"Predicate":"purple", "Class":"color"},
		{"Predicate":"faint", "Class":"color"},
		{"Predicate":"bright", "Class":"color"},
		{"Predicate":"dark", "Class":"color"},
		{"Predicate":"clear", "Class":"color"},
		{"Predicate":"wan", "Class":"color"},	
		{"Predicate":"tubby", "Class":"size"},
		{"Predicate":"fat", "Class":"size"},
		{"Predicate":"narrow", "Class":"size"},
		{"Predicate":"flat", "Class":"size"},
		{"Predicate":"thin", "Class":"size"},
		{"Predicate":"huge", "Class":"size"},
		{"Predicate":"itsy", "Class":"size"},
		{"Predicate":"tall", "Class":"size"},
		{"Predicate":"podgy", "Class":"size"},
		{"Predicate":"small", "Class":"size"},
		{"Predicate":"long", "Class":"size"},
		{"Predicate":"chubby", "Class":"size"},
		{"Predicate":"thick", "Class":"size"},
		{"Predicate":"little", "Class":"size"},
		{"Predicate":"weeny", "Class":"size"},
		{"Predicate":"giant", "Class":"size"},
		{"Predicate":"eensie", "Class":"size"},
		{"Predicate":"teeny", "Class":"size"},
		{"Predicate":"tiny", "Class":"size"},
		{"Predicate":"wee", "Class":"size"},
		{"Predicate":"big", "Class":"size"},
		{"Predicate":"itty", "Class":"size"},
		{"Predicate":"weensie", "Class":"size"},
		{"Predicate":"bitsy", "Class":"size"},
		{"Predicate":"teensie", "Class":"size"},
		{"Predicate":"wide", "Class":"size"},
		{"Predicate":"large", "Class":"size"},
		{"Predicate":"gigantic", "Class":"size"},
		{"Predicate":"short", "Class":"size"},
		{"Predicate":"open", "Class":"size"},
		{"Predicate":"top", "Class":"size"},
		{"Predicate":"even", "Class":"size"},
		{"Predicate":"trim", "Class":"size"},
		{"Predicate":"wooden", "Class":"material"},
		{"Predicate":"plastic", "Class":"material"},
		{"Predicate":"iron", "Class":"material"},
		{"Predicate":"wool", "Class":"material"},
		{"Predicate":"silk", "Class":"material"},
		{"Predicate":"concrete", "Class":"material"},
		{"Predicate":"cheese", "Class":"material"},
		{"Predicate":"egg", "Class":"material"},
		{"Predicate":"flower", "Class":"material"},
		{"Predicate":"lamb", "Class":"material"},
		{"Predicate":"blood", "Class":"material"},
		{"Predicate":"butter", "Class":"material"},
		{"Predicate":"sheep", "Class":"material"},
		{"Predicate":"wine", "Class":"material"},
		{"Predicate":"tooth", "Class":"material"},
		{"Predicate":"fog", "Class":"material"},
		{"Predicate":"lemon", "Class":"material"},
		{"Predicate":"dry", "Class":"texture"},
		{"Predicate":"sharp", "Class":"texture"},
		{"Predicate":"hard", "Class":"texture"},
		{"Predicate":"rough", "Class":"texture"},
		{"Predicate":"damp", "Class":"texture"},
		{"Predicate":"wet", "Class":"texture"},
		{"Predicate":"soft", "Class":"texture"},
		{"Predicate":"smooth", "Class":"texture"},
		//{"Predicate":"haired", "Class":"texture"},
		{"Predicate":"crunch", "Class":"texture"},
		{"Predicate":"warm", "Class":"texture"},
		{"Predicate":"hot", "Class":"texture"},
		{"Predicate":"cold", "Class":"texture"},
		{"Predicate":"fuzzy", "Class":"texture"},
		{"Predicate":"heavy", "Class":"texture"},
		{"Predicate":"bumpy", "Class":"texture"},
		{"Predicate":"fluffy", "Class":"texture"},
		{"Predicate":"whole", "Class":"texture"},
		{"Predicate":"hollow", "Class":"texture"},
		{"Predicate":"empty", "Class":"texture"},
		{"Predicate":"waterproof", "Class":"texture"},
		{"Predicate":"slippery", "Class":"texture"},
		{"Predicate":"crisp", "Class":"texture"},
		{"Predicate":"dense", "Class":"texture"},
		{"Predicate":"full", "Class":"texture"},
		{"Predicate":"skinny", "Class":"texture"},
		{"Predicate":"fragile", "Class":"texture"},
		{"Predicate":"crooked", "Class":"texture"},
		{"Predicate":"strong", "Class":"texture"},
		{"Predicate":"cool", "Class":"texture"},
		{"Predicate":"misshapen", "Class":"texture"},
		{"Predicate":"naked", "Class":"texture"},
		{"Predicate":"clean", "Class":"texture"},
		{"Predicate":"dirty", "Class":"texture"},
		{"Predicate":"steady", "Class":"texture"},
		{"Predicate":"bald", "Class":"texture"},
		{"Predicate":"delicate", "Class":"texture"},?
		{"Predicate":"legged", "Class":"texture"},
		{"Predicate":"horned", "Class":"texture"},
		{"Predicate":"steep", "Class":"texture"},
		{"Predicate":"sparkly", "Class":"texture"},
		{"Predicate":"flutter", "Class":"texture"},
		{"Predicate":"wiggly", "Class":"texture"},
		{"Predicate":"wriggly", "Class":"texture"},
		{"Predicate":"light", "Class":"texture"},
		{"Predicate":"loud", "Class":"texture"},
		{"Predicate":"quiet", "Class":"texture"},
		{"Predicate":"slick", "Class":"texture"},
		{"Predicate":"young", "Class":"age"},
		{"Predicate":"ripe", "Class":"age"},
		{"Predicate":"new", "Class":"age"},
		{"Predicate":"ancient", "Class":"age"},
		{"Predicate":"teenage", "Class":"age"},
		{"Predicate":"old", "Class":"age"},
		{"Predicate":"ole", "Class":"age"},
		{"Predicate":"fresh", "Class":"age"},
		{"Predicate":"good", "Class":"quality"},awful
		{"Predicate":"good", "Class":"quality"},brilliant?
		{"Predicate":"good", "Class":"quality"},fantastic
		{"Predicate":"good", "Class":"quality"},awesome
		{"Predicate":"good", "Class":"quality"},okay
		{"Predicate":"good", "Class":"quality"},good
		{"Predicate":"good", "Class":"quality"},nasty
		{"Predicate":"good", "Class":"quality"},bad
		{"Predicate":"good", "Class":"quality"},horrid
		{"Predicate":"good", "Class":"quality"},horrible
		{"Predicate":"good", "Class":"quality"},nice
		{"Predicate":"good", "Class":"quality"},poor
		{"Predicate":"good", "Class":"quality"},alright
		{"Predicate":"good", "Class":"quality"},great
		{"Predicate":"good", "Class":"quality"},grand
		{"Predicate":"good", "Class":"quality"},crummy
		{"Predicate":"good", "Class":"quality"},odd
		{"Predicate":"good", "Class":"quality"},weird
		{"Predicate":"good", "Class":"quality"},yuck
		{"Predicate":"good", "Class":"quality"},dandy
		{"Predicate":"good", "Class":"quality"},wrong
		{"Predicate":"good", "Class":"quality"},perfect
		{"Predicate":"good", "Class":"quality"},nifty
		{"Predicate":"good", "Class":"quality"},special
		{"Predicate":"good", "Class":"quality"},pathetic
		{"Predicate":"good", "Class":"quality"},correct
		{"Predicate":"good", "Class":"quality"},cute
		{"Predicate":"good", "Class":"quality"},regular
		{"Predicate":"good", "Class":"quality"},fancy
		{"Predicate":"good", "Class":"quality"},comfy
		{"Predicate":"good", "Class":"quality"},right
		{"Predicate":"good", "Class":"quality"},exciting
		{"Predicate":"good", "Class":"quality"},super
		{"Predicate":"good", "Class":"quality"},illustrious
		{"Predicate":"good", "Class":"quality"},cozy
		{"Predicate":"good", "Class":"quality"},gorgeous
		{"Predicate":"good", "Class":"quality"},beautiful
		{"Predicate":"good", "Class":"quality"},precious
		{"Predicate":"good", "Class":"quality"},delicious?
		{"Predicate":"good", "Class":"quality"},yum?
		{"Predicate":"good", "Class":"quality"},darned
		{"Predicate":"good", "Class":"quality"},pretty
		{"Predicate":"good", "Class":"quality"},handy
		{"Predicate":"good", "Class":"quality"},plain
		{"Predicate":"good", "Class":"quality"},ugly
//		{"Predicate":"good", "Class":"quality"},bonnie
		{"Predicate":"good", "Class":"quality"},dulcet
		{"Predicate":"good", "Class":"quality"},dear
		{"Predicate":"good", "Class":"quality"},junk
		{"Predicate":"good", "Class":"quality"},scare
		{"Predicate":"good", "Class":"quality"},proper
		{"Predicate":"good", "Class":"quality"},rotten
//		{"Predicate":"good", "Class":"quality"},choice?
		{"Predicate":"good", "Class":"quality"},strange
		{"Predicate":"good", "Class":"quality"},posh
		{"Predicate":"good", "Class":"quality"},wild
		{"Predicate":"good", "Class":"quality"},fun

		{"Predicate":"round", "Class":"shape"},						
		{"Predicate":"square", "Class":"shape"},
		{"Predicate":"oval", "Class":"shape"},
		{"Predicate":"misshapen", "Class":"shape"}
]);

var nouns = [
		{"Noun":"apple", "NounClass":"food"},
		{"Noun":"banana", "NounClass":"food"},
		{"Noun":"carrot", "NounClass":"food"},
		{"Noun":"cheese", "NounClass":"food"},
		{"Noun":"tomato", "NounClass":"food"},								
		{"Noun":"chair", "NounClass":"furniture"},								
		{"Noun":"couch", "NounClass":"furniture"},								
		{"Noun":"fan", "NounClass":"furniture"},								
		{"Noun":"TV", "NounClass":"furniture"},								
		{"Noun":"desk", "NounClass":"furniture"}								
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	for (var i=0; i<adjectives.length; i++) {
		// noun = _.sample(nouns);
		stims.push(
			{
				"Predicate":adjectives[i].Predicate,
				"Class":adjectives[i].Class,				
				// "Noun":noun.Noun,
				// "NounClass":noun.NounClass
			}
			);
		}
		
	return stims;
	
}