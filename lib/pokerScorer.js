// PokerScorer.js
// Adapted from Subskybox
// http://www.codeproject.com/Articles/569271/A-Poker-hand-analyzer-in-JavaScript-using-bit-math

// Takes two list of int representing the ranks and suits of a hand.
//  Ranks: A=14, K=13, Q=12, J=11 ...
//  Suits: Spades=1, Clubs=2, Hearts=4, Diamonds=8

// Usage:
//  toPokerElm(cards)( [ 10, 11, 12, 13, 14], [1, 1, 1, 1, 1 ] );
//  --> { score: 10411194, description: "Royal flush!! holy sh*t!" }
//  Min Score 484,402; Max Score 10,411,194


function getResult(cards) {
    try {
        var score = get5cardScore(cards[0], cards[1]);
        var description = getTypeDetail(score)
        }
    catch (err) {
      throw "Error: " + cards + " passed to toPokerElm. Required: [[ints], [ints]]"
    }
    return { score: score
           , description: description
           }
}


var names = [0,0,2,3,4,5,6,7,8,9,10,"Jack","Queen","King","Ace"];

function get5cardScore(cs,ss){ //calculates the equivalence score of 5 cards
	var v, i, o, c, d={}, s = 1<<cs[0]|1<<cs[1]|1<<cs[2]|1<<cs[3]|1<<cs[4];
  	for (i=v=o=0; i<5; i++) {v += (o=Math.pow(2,cs[i]*4))*(d[cs[i]] = (v/o&15)+1);}
  	v = v%15-((s/(s&-s)==31)||(s==0x403c)?3:1)-(ss[0]==(ss[1]|ss[2]|ss[3]|ss[4]))*((s==0x7c00)?-5:1);
    c = (s==0x403c)?[5,4,3,2,1]:cs.slice().sort(function(a,b){return (d[a]<d[b])?1:(d[a]>d[b])?-1:(b-a);});
	return [7,8,4,5,0,1,2,9,3,6][v]<<20|c[0]<<16|c[1]<<12|c[2]<<8|c[3]<<4|c[4];
}

function getTypeDetail(x){
	var cat = x>>20, c1 = x>>16&15, c3 = x>>8&15, c4 = x>>4&15;

	switch(cat){
		case 0: return names[c1] + " high";
		case 1: return "Pair of "+names[c1]+"s";
		case 2: return "Two pairs, "+names[c1]+"s and "+names[c3]+"s";
		case 3: return "Three of a kind, "+names[c1]+"s";
		case 4: return "Straight " +names[c1]+ " high";
		case 5: return "Flush " +names[c1]+" high";
		case 6: return "Full House, " + names[c1] + "s full of "+names[c4]+"s";
		case 7: return "Four of a kind, "+names[c1]+"s";
		case 8: return "Straight flush! "+ names[c1]+" high";
		case 9: return "Royal flush!! holy sh*t!";
	}
}


