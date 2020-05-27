const teams = [
  "West Ham",
  "Tottenham",
  "Man City",
  "Man Utd",
  "Everton",
  "Norwich",
  "Chelsea",
  "Leicester",
  "Liverpool",
  "Arsenal",
];

const matches = [];

for (let i = 0; i < teams.length; i++) {
  for (let j = i + 1; j < teams.length; j++) {
    matches.push([teams[i], teams[j]]);
  }
}

const getTeamsFromMatches = (matches) => matches.flat();

const getRound = (teams, matches) =>
  teams.reduce((acc, team) => {
    const currentTeams = getTeamsFromMatches(acc);
    if (currentTeams.includes(team)) {
      return acc;
    }
    return [
      ...acc,
      matches.find(
        (match) =>
          (match[1] === team && !currentTeams.includes(match[0])) ||
          (match[0] === team && !currentTeams.includes(match[1]))
      ),
    ];
  }, []);

const getRounds = (teams, matches) => {
  const rounds = [];
  while (matches.length) {
    const round = getRound(teams, matches);
    matches = matches.filter((el) => !round.includes(el));
    rounds.push(round);
  }
  return rounds;
};
console.log(getRounds(teams, matches));
