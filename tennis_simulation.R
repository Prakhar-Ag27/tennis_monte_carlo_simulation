simulate_match <- function(djokovic_prob_set1_2, djokovic_prob_fatigue, 
                           federer_prob_set1_2, federer_prob_fatigue, 
                           num_simulations = 1000) {
  
  # Function to simulate a single point in a game
  simulate_point <- function(server_prob) {
    if (runif(1) < server_prob) {
      return("Server")  # Server wins the point
    } else {
      return("Receiver")  # Receiver wins the point
    }
  }
  
  # Function to simulate a single game
  simulate_game <- function(server_prob, receiver_prob) {
    server_points <- 0
    receiver_points <- 0
    
    while (TRUE) {
      # Simulate the next point in the game
      point_winner <- simulate_point(server_prob)
      
      if (point_winner == "Server") {
        server_points <- server_points + 1
      } else {
        receiver_points <- receiver_points + 1
      }
      
      # Check if someone won the game (needs 4 points and a 2-point lead)
      if ((server_points >= 4 && server_points - receiver_points >= 2)) {
        return("Server")
      } else if ((receiver_points >= 4 && receiver_points - server_points >= 2)) {
        return("Receiver")
      }
    }
  }
  
  # Function to simulate a single set
  simulate_set <- function(djokovic_prob, federer_prob, starting_server) {
    djokovic_games <- 0
    federer_games <- 0
    
    # Set the server to the random starting server
    server <- starting_server
    
    while (TRUE) {
      # Simulate a game with alternating servers
      if (server == "Djokovic") {
        game_winner <- simulate_game(djokovic_prob, 1 - djokovic_prob)  # Djokovic serves
      } else {
        game_winner <- simulate_game(federer_prob, 1 - federer_prob)  # Federer serves
      }
      
      # Update game count based on the winner
      if (game_winner == "Server") {
        if (server == "Djokovic") {
          djokovic_games <- djokovic_games + 1
        } else {
          federer_games <- federer_games + 1
        }
      } else {
        if (server == "Djokovic") {
          federer_games <- federer_games + 1
        } else {
          djokovic_games <- djokovic_games + 1
        }
      }
      
      # Alternate server for the next game
      server <- ifelse(server == "Djokovic", "Federer", "Djokovic")
      
      # Check if someone won the set (6 games with 2-game lead)
      if ((djokovic_games >= 6 || federer_games >= 6) &&
          abs(djokovic_games - federer_games) >= 2) {
        break
      }
    }
    return(ifelse(djokovic_games > federer_games, "Djokovic", "Federer"))
  }
  
  # Simulate a single match
  simulate_single_match <- function(djokovic_prob_set1_2, federer_prob_set1_2, djokovic_prob_fatigue, federer_prob_fatigue, fatigue_effect = TRUE) {
    djokovic_sets <- 0
    federer_sets <- 0
    
    # Randomly choose the first server for the match
    first_server <- ifelse(runif(1) < 0.5, "Djokovic", "Federer")
    
    for (set in 1:5) {
      # Use the appropriate probabilities based on set number
      if (set <= 2) {
        winner <- simulate_set(djokovic_prob_set1_2, federer_prob_set1_2, first_server)
      } else {
        if (fatigue_effect) {
          winner <- simulate_set(djokovic_prob_fatigue, federer_prob_fatigue, first_server)
        } else {
          winner <- simulate_set(djokovic_prob_set1_2, federer_prob_set1_2, first_server)  # Use the same probabilities as before
        }
      }
      
      if (winner == "Djokovic") {
        djokovic_sets <- djokovic_sets + 1
      } else {
        federer_sets <- federer_sets + 1
      }
      
      # Check if the match is over (best of 5 sets)
      if (djokovic_sets == 3 || federer_sets == 3) {
        break
      }
    }
    return(ifelse(djokovic_sets == 3, "Djokovic", "Federer"))
  }
  
  # Run the simulations with fatigue effects
  results_with_fatigue <- replicate(num_simulations, simulate_single_match(djokovic_prob_set1_2, federer_prob_set1_2, djokovic_prob_fatigue, federer_prob_fatigue, fatigue_effect = TRUE))
  djokovic_wins_with_fatigue <- sum(results_with_fatigue == "Djokovic")
  federer_wins_with_fatigue <- sum(results_with_fatigue == "Federer")
  
  # Run the simulations without fatigue effects
  results_without_fatigue <- replicate(num_simulations, simulate_single_match(djokovic_prob_set1_2, federer_prob_set1_2, djokovic_prob_fatigue, federer_prob_fatigue, fatigue_effect = FALSE))
  djokovic_wins_without_fatigue <- sum(results_without_fatigue == "Djokovic")
  federer_wins_without_fatigue <- sum(results_without_fatigue == "Federer")
  
  # Return both results
  return(list(
    With_Fatigue = list(
      Djokovic_Wins = djokovic_wins_with_fatigue,
      Federer_Wins = federer_wins_with_fatigue,
      Djokovic_Win_Percentage = djokovic_wins_with_fatigue / num_simulations * 100,
      Federer_Win_Percentage = federer_wins_with_fatigue / num_simulations * 100
    ),
    Without_Fatigue = list(
      Djokovic_Wins = djokovic_wins_without_fatigue,
      Federer_Wins = federer_wins_without_fatigue,
      Djokovic_Win_Percentage = djokovic_wins_without_fatigue / num_simulations * 100,
      Federer_Win_Percentage = federer_wins_without_fatigue / num_simulations * 100
    )
  ))
}

# Example usage
set.seed(123) # For reproducibility
result <- simulate_match(
  djokovic_prob_set1_2 = 0.698553, 
  djokovic_prob_fatigue = 0.713460,
  federer_prob_set1_2 = 0.703125,
  federer_prob_fatigue = 0.651163,
  num_simulations = 10000
)

print(result)
