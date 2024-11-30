simulate_match <- function(djokovic_prob_first_serve_no_fatigue, djokovic_prob_return_no_fatigue,
                           federer_prob_first_serve_no_fatigue, federer_prob_return_no_fatigue,
                           djokovic_prob_first_serve_with_fatigue, djokovic_prob_return_with_fatigue,
                           federer_prob_first_serve_with_fatigue, federer_prob_return_with_fatigue,
                           djokovic_first_serve_percentage, federer_first_serve_percentage,
                           num_simulations = 1000) {
  
  # Function to simulate a single point in a game
  simulate_point <- function(server_first_serve_prob, server_return_prob, first_serve_percentage) {
    # Determine if the server makes the first serve
    if (runif(1) < first_serve_percentage) {
      # If first serve is successful, use the server's first serve win probability
      point_winner <- ifelse(runif(1) < server_first_serve_prob, "Server", "Receiver")
    } else {
      # If first serve is a fault, use the returner's return win probability for the second serve
      point_winner <- ifelse(runif(1) < server_return_prob, "Receiver", "Server")
    }
    return(point_winner)
  }
  
  # Function to simulate a single game
  simulate_game <- function(server_first_serve_prob, server_return_prob, first_serve_percentage) {
    server_points <- 0
    receiver_points <- 0
    
    while (TRUE) {
      # Simulate the next point in the game
      point_winner <- simulate_point(server_first_serve_prob, server_return_prob, first_serve_percentage)
      
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
  simulate_set <- function(set_number, djokovic_first_serve_prob_no_fatigue, federer_first_serve_prob_no_fatigue,
                           djokovic_return_prob_no_fatigue, federer_return_prob_no_fatigue,
                           djokovic_first_serve_prob_with_fatigue, federer_first_serve_prob_with_fatigue,
                           djokovic_return_prob_with_fatigue, federer_return_prob_with_fatigue,
                           djokovic_first_serve_percentage, federer_first_serve_percentage, starting_server) {
    
    # Use different probabilities based on whether fatigue is considered
    if (set_number <= 2) {
      # No fatigue, use the initial probabilities
      djokovic_first_serve_prob <- djokovic_first_serve_prob_no_fatigue
      federer_first_serve_prob <- federer_first_serve_prob_no_fatigue
      djokovic_return_prob <- djokovic_return_prob_no_fatigue
      federer_return_prob <- federer_return_prob_no_fatigue
    } else {
      # Fatigue is considered, use the adjusted probabilities
      djokovic_first_serve_prob <- djokovic_first_serve_prob_with_fatigue
      federer_first_serve_prob <- federer_first_serve_prob_with_fatigue
      djokovic_return_prob <- djokovic_return_prob_with_fatigue
      federer_return_prob <- federer_return_prob_with_fatigue
    }
    
    djokovic_games <- 0
    federer_games <- 0
    
    # Set the server to the random starting server
    server <- starting_server
    
    while (TRUE) {
      # Simulate a game with alternating servers
      if (server == "Djokovic") {
        game_winner <- simulate_game(djokovic_first_serve_prob, federer_return_prob, djokovic_first_serve_percentage)  # Djokovic serves
      } else {
        game_winner <- simulate_game(federer_first_serve_prob, djokovic_return_prob, federer_first_serve_percentage)  # Federer serves
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
  simulate_single_match <- function() {
    djokovic_sets <- 0
    federer_sets <- 0
    
    # Randomly choose the first server for the match
    first_server <- ifelse(runif(1) < 0.5, "Djokovic", "Federer")
    
    for (set in 1:5) {
      # Use the appropriate probabilities based on set number
      winner <- simulate_set(set, djokovic_prob_first_serve_no_fatigue, federer_prob_first_serve_no_fatigue, 
                             djokovic_prob_return_no_fatigue, federer_prob_return_no_fatigue,
                             djokovic_prob_first_serve_with_fatigue, federer_prob_first_serve_with_fatigue,
                             djokovic_prob_return_with_fatigue, federer_prob_return_with_fatigue,
                             djokovic_first_serve_percentage, federer_first_serve_percentage, first_server)
      
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
  
  # Run the simulations
  results <- replicate(num_simulations, simulate_single_match())
  djokovic_wins <- sum(results == "Djokovic")
  federer_wins <- sum(results == "Federer")
  
  # Return both results
  return(list(
    Djokovic_Wins = djokovic_wins,
    Federer_Wins = federer_wins,
    Djokovic_Win_Percentage = djokovic_wins / num_simulations * 100,
    Federer_Win_Percentage = federer_wins / num_simulations * 100
  ))
}

# Example usage
set.seed(123) # For reproducibility
result <- simulate_match(
  djokovic_prob_first_serve_no_fatigue = 0.698871,  
  djokovic_prob_return_no_fatigue = 0.409978,
  federer_prob_first_serve_no_fatigue = 0.708260,   
  federer_prob_return_no_fatigue = 0.419714,        
  
  djokovic_prob_first_serve_with_fatigue = 0.701003,  # Fatigue effect for Djokovic
  djokovic_prob_return_with_fatigue = 0.436573,
  federer_prob_first_serve_with_fatigue = 0.724308,   # Fatigue effect for Federer
  federer_prob_return_with_fatigue = 0.386384,        
  
  djokovic_first_serve_percentage = 0.637,  
  federer_first_serve_percentage = 0.62,    
  num_simulations = 10000
)

print(result)
