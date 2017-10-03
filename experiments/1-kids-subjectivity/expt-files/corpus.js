// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"red", "Class":"color"},
		{"Predicate":"yellow", "Class":"color"},
		{"Predicate":"green", "Class":"color"},
		{"Predicate":"blue", "Class":"color"},
		{"Predicate":"purple", "Class":"color"},
		{"Predicate":"brown", "Class":"color"},
		{"Predicate":"gold", "Class":"color"},
		{"Predicate":"golden", "Class":"color"},
		{"Predicate":"tan", "Class":"color"},
		{"Predicate":"pink", "Class": "color"},
		{"Predicate":"orange", "Class":"color"},
		{"Predicate":"silver", "Class":"color"},
		{"Predicate":"white", "Class":"color"},
		{"Predicate":"grey", "Class":"color"},
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
		{"Predicate":"young", "Class":"age"},
		{"Predicate":"ripe", "Class":"age"},
		{"Predicate":"new", "Class":"age"},
		{"Predicate":"ancient", "Class":"age"},
		{"Predicate":"teenage", "Class":"age"},
		{"Predicate":"old", "Class":"age"},
		{"Predicate":"fresh", "Class":"age"},
		{"Predicate":"wooden", "Class":"material"},
		{"Predicate":"plastic", "Class":"material"},
		{"Predicate":"iron", "Class":"material"},
		{"Predicate":"wool", "Class":"material"},
		{"Predicate":"silk", "Class":"material"},
		{"Predicate":"concrete", "Class":"material"},
		{"Predicate":"haired", "Class":"material"},
		{"Predicate":"awful", "Class":"value"},
		{"Predicate":"brilliant", "Class":"value"},
		{"Predicate":"fantastic", "Class":"value"},
		{"Predicate":"awesome", "Class":"value"},
		{"Predicate":"okay", "Class":"value"},
		{"Predicate":"good", "Class":"value"},
		{"Predicate":"nasty", "Class":"value"},
		{"Predicate":"bad", "Class":"value"},
		{"Predicate":"horrid", "Class":"value"},
		{"Predicate":"horrible", "Class":"value"},
		{"Predicate":"nice", "Class":"value"},
		{"Predicate":"poor", "Class":"value"},
		{"Predicate":"alright", "Class":"value"},
		{"Predicate":"great", "Class":"value"},
		{"Predicate":"grand", "Class":"value"},
		{"Predicate":"crummy", "Class":"value"},
		{"Predicate":"odd", "Class":"value"},
		{"Predicate":"weird", "Class":"value"},
		{"Predicate":"yucky", "Class":"value"},
		{"Predicate":"dandy", "Class":"value"},
		{"Predicate":"wrong", "Class":"value"},
		{"Predicate":"perfect", "Class":"value"},
		{"Predicate":"nifty", "Class":"value"},
		{"Predicate":"special", "Class":"value"},
		{"Predicate":"pathetic", "Class":"value"},
		{"Predicate":"correct", "Class":"value"},
		{"Predicate":"cute", "Class":"value"},
		{"Predicate":"regular", "Class":"value"},
		{"Predicate":"fancy", "Class":"value"},
		{"Predicate":"comfy", "Class":"value"},
		{"Predicate":"right", "Class":"value"},
		{"Predicate":"exciting", "Class":"value"},
		{"Predicate":"super", "Class":"value"},
		{"Predicate":"illustrious", "Class":"value"},
		{"Predicate":"cozy", "Class":"value"},
		{"Predicate":"gorgeous", "Class":"value"},
		{"Predicate":"beautiful", "Class":"value"},
		{"Predicate":"precious", "Class":"value"},
		{"Predicate":"delicious", "Class":"value"},
		{"Predicate":"yummy", "Class":"value"},
		{"Predicate":"tidy", "Class":"value"},
		{"Predicate":"pretty", "Class":"value"},
		{"Predicate":"handy", "Class":"value"},
		{"Predicate":"plain", "Class":"value"},
		{"Predicate":"ugly", "Class":"value"},
		{"Predicate":"bonnie", "Class":"value"},
		{"Predicate":"dulcet", "Class":"value"},
		{"Predicate":"dear", "Class":"value"},
		{"Predicate":"junk", "Class":"value"},
		{"Predicate":"scary", "Class":"value"},
		{"Predicate":"proper", "Class":"value"},
		{"Predicate":"rotten", "Class":"value"},
		{"Predicate":"poop", "Class":"value"},
		{"Predicate":"strange", "Class":"value"},
		{"Predicate":"posh", "Class":"value"},
		{"Predicate":"wild", "Class":"value"},
		{"Predicate":"fun", "Class":"value"},
		{"Predicate":"neat", "Class":"value"},
		{"Predicate":"just", "Class":"value"},
		{"Predicate":"round", "Class":"shape"},
		{"Predicate":"oval", "Class":"shape"},
		{"Predicate":"square", "Class":"shape"},
		{"Predicate":"squiggly", "Class":"shape"},
		{"Predicate":"tubby", "Class":"dimension"},
		{"Predicate":"fat", "Class":"dimension"},
		{"Predicate":"narrow", "Class":"dimension"},
		{"Predicate":"flat", "Class":"dimension"},
		{"Predicate":"thin", "Class":"dimension"},
		{"Predicate":"huge", "Class":"dimension"},
		{"Predicate":"itsy", "Class":"dimension"},
		{"Predicate":"tall", "Class":"dimension"},
		{"Predicate":"podgy", "Class":"dimension"},
		{"Predicate":"small", "Class":"dimension"},
		{"Predicate":"long", "Class":"dimension"},
		{"Predicate":"chubby", "Class":"dimension"},
		{"Predicate":"thick", "Class":"dimension"},
		{"Predicate":"little", "Class":"dimension"},
		{"Predicate":"weeny", "Class":"dimension"},
		{"Predicate":"giant", "Class":"dimension"},
		{"Predicate":"eensie", "Class":"dimension"},
		{"Predicate":"teeny", "Class":"dimension"},
		{"Predicate":"tiny", "Class":"dimension"},
		{"Predicate":"wee", "Class":"dimension"},
		{"Predicate":"big", "Class":"dimension"},
		{"Predicate":"itty", "Class":"dimension"},
		{"Predicate":"weensie", "Class":"dimension"},
		{"Predicate":"bitsy", "Class":"dimension"},
		{"Predicate":"teensie", "Class":"dimension"},
		{"Predicate":"wide", "Class":"dimension"},
		{"Predicate":"large", "Class":"dimension"},
		{"Predicate":"gigantic", "Class":"dimension"},
		{"Predicate":"short", "Class":"dimension"},
		{"Predicate":"open", "Class":"dimension"},
		{"Predicate":"trim", "Class":"dimension"},
		{"Predicate":"skinny", "Class":"dimension"},
		{"Predicate":"dry", "Class":"physical"},
		{"Predicate":"sharp", "Class":"physical"},
		{"Predicate":"hard", "Class":"physical"},
		{"Predicate":"rough", "Class":"physical"},
		{"Predicate":"damp", "Class":"physical"},
		{"Predicate":"wet", "Class":"physical"},
		{"Predicate":"soft", "Class":"physical"},
		{"Predicate":"smooth", "Class":"physical"},
		{"Predicate":"crunchy", "Class":"physical"},
		{"Predicate":"warm", "Class":"physical"},
		{"Predicate":"hot", "Class":"physical"},
		{"Predicate":"cold", "Class":"physical"},
		{"Predicate":"fuzzy", "Class":"physical"},
		{"Predicate":"heavy", "Class":"physical"},
		{"Predicate":"bumpy", "Class":"physical"},
		{"Predicate":"fluffy", "Class":"physical"},
		{"Predicate":"salty", "Class":"physical"},
		{"Predicate":"hollow", "Class":"physical"},
		{"Predicate":"empty", "Class":"physical"},
		{"Predicate":"waterproof", "Class":"physical"},
		{"Predicate":"slippery", "Class":"physical"},
		{"Predicate":"crisp", "Class":"physical"},
		{"Predicate":"dense", "Class":"physical"},
		{"Predicate":"full", "Class":"physical"},
		{"Predicate":"sweet", "Class":"physical"},
		{"Predicate":"fragile", "Class":"physical"},
		{"Predicate":"crooked", "Class":"physical"},
		{"Predicate":"strong", "Class":"physical"},
		{"Predicate":"cool", "Class":"physical"},
		{"Predicate":"misshapen", "Class":"physical"},
		{"Predicate":"spicey", "Class":"physical"},
		{"Predicate":"clean", "Class":"physical"},
		{"Predicate":"dirty", "Class":"physical"},
		{"Predicate":"shiny", "Class":"physical"},
		{"Predicate":"bald", "Class":"physical"},
		{"Predicate":"delicate", "Class":"physical"},
		{"Predicate":"squeaky", "Class":"physical"},
		{"Predicate":"steep", "Class":"physical"},
		{"Predicate":"sparkly", "Class":"physical"},
		{"Predicate":"light", "Class":"physical"},
		{"Predicate":"loud", "Class":"physical"},
		{"Predicate":"quiet", "Class":"physical"},
		{"Predicate":"slick", "Class":"physical"},
		{"Predicate":"far", "Class":"location"},
		{"Predicate":"western", "Class":"location"},
		{"Predicate":"front", "Class":"location"},
		{"Predicate":"south", "Class":"location"},
		{"Predicate":"east", "Class":"location"},
		{"Predicate":"side", "Class":"location"},
		{"Predicate":"upstairs", "Class":"location"},
		{"Predicate":"left", "Class":"location"},
		{"Predicate":"back", "Class":"location"},
		{"Predicate":"bottom", "Class":"location"},
		{"Predicate":"central", "Class":"location"},
		{"Predicate":"polar", "Class":"location"},
		{"Predicate":"high", "Class":"location"},
		{"Predicate":"low", "Class":"location"},
		{"Predicate":"glottal", "Class":"location"},
		{"Predicate":"meadow", "Class":"location"},
		{"Predicate":"close", "Class":"location"},
		{"Predicate":"top", "Class":"location"},
		{"Predicate":"Chinese", "Class":"nationality"},
		{"Predicate":"French", "Class":"nationality"},
		{"Predicate":"Mexican", "Class":"nationality"},
		{"Predicate":"Dutch", "Class":"nationality"},
		{"Predicate":"American", "Class":"nationality"},
		{"Predicate":"English", "Class":"nationality"},
		{"Predicate":"quick", "Class":"speed"},
		{"Predicate":"instant", "Class":"speed"},
		{"Predicate":"slow", "Class":"speed"},
		{"Predicate":"fast", "Class":"speed"},
		{"Predicate":"speedy", "Class":"speed"},
		{"Predicate":"sleepy", "Class":"human"},
		{"Predicate":"sorry", "Class":"human"},
		{"Predicate":"brave", "Class":"human"},
		{"Predicate":"angry", "Class":"human"},
		{"Predicate":"live", "Class":"human"},
		{"Predicate":"clever", "Class":"human"},
		{"Predicate":"mean", "Class":"human"},
		{"Predicate":"vicious", "Class":"human"},
		{"Predicate":"naughty", "Class":"human"},
		{"Predicate":"dumb", "Class":"human"},
		{"Predicate":"stupid", "Class":"human"},
		{"Predicate":"wicked", "Class":"human"},
		{"Predicate":"maternal", "Class":"human"},
		{"Predicate":"jolly", "Class":"human"},
		{"Predicate":"gentle", "Class":"human"},
		{"Predicate":"innocent", "Class":"human"},
		{"Predicate":"crazy", "Class":"human"},
		{"Predicate":"healthy", "Class":"human"},
		{"Predicate":"dead", "Class":"human"},
		{"Predicate":"gloomy", "Class":"human"},
		{"Predicate":"silly", "Class":"human"},
		{"Predicate":"goofy", "Class":"human"},
		{"Predicate":"hungry", "Class":"human"},
		{"Predicate":"sad", "Class":"human"},
		{"Predicate":"merry", "Class":"human"},
		{"Predicate":"attentive", "Class":"human"},
		{"Predicate":"gracious", "Class":"human"},
		{"Predicate":"whiny", "Class":"human"},
		{"Predicate":"dizzy", "Class":"human"},
		{"Predicate":"cross", "Class":"human"},
		{"Predicate":"cranky", "Class":"human"},
		{"Predicate":"fierce", "Class":"human"},
		{"Predicate":"human", "Class":"human"},
		{"Predicate":"lazy", "Class":"human"},
		{"Predicate":"greedy", "Class":"human"},
		{"Predicate":"vain", "Class":"human"},
		{"Predicate":"smart", "Class":"human"},
		{"Predicate":"happy", "Class":"human"},
		{"Predicate":"grouchy", "Class":"human"},
		{"Predicate":"smiley", "Class":"human"},
		{"Predicate":"royal", "Class":"human"},
		{"Predicate":"fair", "Class":"human"},
		{"Predicate":"grumpy", "Class":"human"},
		{"Predicate":"mad", "Class":"human"},
		{"Predicate":"wise", "Class":"human"},
		{"Predicate":"kind", "Class":"human"},
		{"Predicate":"sneaky", "Class":"human"},
		{"Predicate":"famous", "Class":"human"},
		{"Predicate":"past", "Class":"temporal"},
		{"Predicate":"next", "Class":"temporal"},
		{"Predicate":"late", "Class":"temporal"},
		{"Predicate":"third", "Class":"temporal"},
		{"Predicate":"final", "Class":"temporal"},
		{"Predicate":"first", "Class":"temporal"},
		{"Predicate":"last", "Class":"temporal"},
		{"Predicate":"early", "Class":"temporal"},
		{"Predicate":"second", "Class":"temporal"},
		{"Predicate":"glitzy", "Class":"other"},
		{"Predicate":"exact", "Class":"other"},
		{"Predicate":"bubbly", "Class":"other"},
		{"Predicate":"organic", "Class":"other"},
		{"Predicate":"busy", "Class":"other"},
		{"Predicate":"natural", "Class":"other"},
		{"Predicate":"fake", "Class":"other"},
		{"Predicate":"sore", "Class":"other"},
		{"Predicate":"pretend", "Class":"other"},
		{"Predicate":"obvious", "Class":"other"},
		{"Predicate":"bumpity", "Class":"other"},
		{"Predicate":"formal", "Class":"other"},
		{"Predicate":"messy", "Class":"other"},
		{"Predicate":"pokey", "Class":"other"},
		{"Predicate":"personal", "Class":"other"},
		{"Predicate":"darned", "Class":"other"},
		{"Predicate":"free", "Class":"other"},
		{"Predicate":"even", "Class":"other"},
		{"Predicate":"double", "Class":"other"},
		{"Predicate":"choice", "Class":"other"},
		{"Predicate":"whole", "Class":"other"},
		{"Predicate":"naked", "Class":"other"},
		{"Predicate":"steady", "Class":"other"},
		{"Predicate":"medical", "Class":"other"},
		{"Predicate":"trick", "Class":"other"},
		{"Predicate":"stinky", "Class":"other"},
		{"Predicate":"horned", "Class":"other"},
		{"Predicate":"ready", "Class":"other"},
		{"Predicate":"different", "Class":"other"},
		{"Predicate":"same", "Class":"other"},
		{"Predicate":"musical", "Class":"other"},
		{"Predicate":"real", "Class":"other"},
		{"Predicate":"wiggly", "Class":"other"},
		{"Predicate":"wriggly", "Class":"other"},
		{"Predicate":"verbal", "Class":"other"},
		{"Predicate":"own", "Class":"other"},
		{"Predicate":"head", "Class":"other"},
		{"Predicate":"true", "Class":"other"},
		{"Predicate":"sick", "Class":"other"},
		{"Predicate":"roundabout", "Class":"other"},
		{"Predicate":"still", "Class":"other"},
		{"Predicate":"spare", "Class":"other"},
		{"Predicate":"part", "Class":"other"},
		{"Predicate":"fellow", "Class":"other"},
		{"Predicate":"undercover", "Class":"other"},
		{"Predicate":"multiple", "Class":"other"}
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