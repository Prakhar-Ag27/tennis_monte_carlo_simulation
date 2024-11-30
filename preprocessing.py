import pandas as pd
import os
import matplotlib.pyplot as plt

# Set the path to your tennis dataset directory
dataset_path = os.path.expanduser(r"C:\Users\Prakhar\Desktop\mcsm\tennis_slam_pointbypoint-master")

# Define years and players of interest
years = [2021, 2022, 2023, 2024]
players = ["Roger Federer", "Novak Djokovic"]

# Function to read matches and points for all slams in a year
def read_data_for_year(year, dataset_path):
    slams = ["ausopen", "frenchopen", "usopen", "wimbledon"]
    match_data = pd.DataFrame()
    points_data = pd.DataFrame()
    
    for slam in slams:
        matches_file = f"{year}-{slam}-matches.csv"
        points_file = f"{year}-{slam}-points.csv"
        
        matches_path = os.path.join(dataset_path, matches_file)
        points_path = os.path.join(dataset_path, points_file)
        
        if os.path.exists(matches_path):
            match_data = pd.concat([match_data, pd.read_csv(matches_path)])
        if os.path.exists(points_path):
            points_data = pd.concat([points_data, pd.read_csv(points_path)])
    
    return match_data, points_data

# Combine data for all years
all_matches = pd.DataFrame()
all_points = pd.DataFrame()

for year in years:
    match_data, points_data = read_data_for_year(year, dataset_path)
    all_matches = pd.concat([all_matches, match_data])
    all_points = pd.concat([all_points, points_data])

# Filter matches for Federer and Djokovic
filtered_matches = all_matches[
    (all_matches["player1"].isin(players)) | (all_matches["player2"].isin(players))
]

# Merge points data with filtered matches using match_id
merged_data = all_points.merge(filtered_matches, on="match_id", how="inner")

# Function to calculate serve win probability
def calculate_serve_win(data, player_name, server_col, point_winner_col, player1_col, player2_col):
    # Identify points where the opponent serves
    data["playerNo"] = data[player1_col].apply(lambda x: 1 if x == player_name else 2)
   
    numerator = len(data[(data['playerNo'] == data['PointWinner']) & (data['PointServer'] == data['PointWinner']) & (data["SetNo"].isin([1, 2]))])
    denominator = len(data[(data['playerNo'] == data['PointServer']) & (data["SetNo"].isin([1, 2]))])
    dic = {}
    dic["nonfatigue"] = numerator / denominator
    
    numerator = len(data[(data['playerNo'] == data['PointWinner']) & (data['PointServer'] == data['PointWinner']) & (~data["SetNo"].isin([1, 2]))])
    denominator = len(data[(data['playerNo'] == data['PointServer']) & (~data["SetNo"].isin([1, 2]))])
    
    dic["fatigue"] = numerator / denominator
    
    return dic

# Function to calculate return win probability
def calculate_return_win(data, player_name, server_col, point_winner_col, player1_col, player2_col):
    # Identify points where the opponent serves
    data["playerNo"] = data[player1_col].apply(lambda x: 1 if x == player_name else 2)
   
    numerator = len(data[(data['playerNo'] == data['PointWinner']) & (data['PointServer'] != data['PointWinner']) & (data["SetNo"].isin([1, 2]))])
    denominator = len(data[(data['playerNo'] != data['PointServer']) & (data["SetNo"].isin([1, 2]))])
    
    dic = {}
    dic["nonfatigue"] = numerator / denominator
    
    numerator = len(data[(data['playerNo'] == data['PointWinner']) & (data['PointServer'] != data['PointWinner']) & (~data["SetNo"].isin([1, 2]))])
    denominator = len(data[(data['playerNo'] != data['PointServer']) & (~data["SetNo"].isin([1, 2]))])
    
    dic["fatigue"] = numerator / denominator
    
    return dic

# Function to calculate break conversion probability
def calculate_break_convert(data, player_name, player1_col, player2_col):
    # Determine if the target player is P1 or P2
    data["playerNo"] = data[player1_col] == player_name

    # Dynamically calculate break points won and attempted
    data["BreakPointsWon"] = data.apply(
        lambda row: row["P1BreakPointWon"] if row["playerNo"] else row["P2BreakPointWon"], axis=1
    )
    data["BreakPointsMissed"] = data.apply(
        lambda row: row["P1BreakPointMissed"] if row["playerNo"] else row["P2BreakPointMissed"], axis=1
    )
    data["BreakPointsAttempted"] = data["BreakPointsWon"] + data["BreakPointsMissed"]

    # Avoid division by zero
    valid_data = data[data["BreakPointsAttempted"] > 0]
    break_conversion_rate = valid_data["BreakPointsWon"].sum() / valid_data["BreakPointsAttempted"].sum()
    return break_conversion_rate

# Calculate probabilities for Federer and Djokovic
results = {}
for player in players:
    player_data = merged_data[
        (merged_data["player1"] == player) | (merged_data["player2"] == player)
    ]
    
    serve_win_rate = calculate_serve_win(
        player_data, player, "PointServer", "PointWinner", "player1", "player2"
    )
    return_win_rate = calculate_return_win(
        player_data, player, "PointServer", "PointWinner", "player1", "player2"
    )
    break_conversion_rate = calculate_break_convert(
        player_data, player, "player1", "player2"
    )
    
    results[player] = {
        "Serve Win Probability (Nonfatigue)": serve_win_rate["nonfatigue"],
        "Serve Win Probability (Fatigue)": serve_win_rate["fatigue"],
        "Return Win Probability (Nonfatigue)": return_win_rate["nonfatigue"],
        "Return Win Probability (Fatigue)": return_win_rate["fatigue"],
        "Break Conversion Probability": break_conversion_rate
    }

# Convert results to a DataFrame for visualization
results_df = pd.DataFrame.from_dict(results, orient="index")
results_df.index.name = "Player"

# Display results as a table (formatted)
print("\nFinal Results (Formatted Table):")
print(results_df.to_string(index=True))

# Visualize the DataFrame
results_df.plot(kind="bar", figsize=(10, 6), alpha=0.75, colormap="viridis")
plt.title("Performance Probabilities of Federer and Djokovic (2013–2016)")
plt.ylabel("Probability")
plt.xlabel("Player")
plt.xticks(rotation=0)
plt.legend(loc="best")
plt.grid(axis="y", linestyle="--", alpha=0.7)
plt.tight_layout()
plt.show()
