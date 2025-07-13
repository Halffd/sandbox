import requests
import json

def revoke_twitch_token(client_id, token):
    """
    Revoke a Twitch OAuth token
    """
    url = "https://id.twitch.tv/oauth2/revoke"
    
    data = {
        'client_id': client_id,
        'token': token
    }
    
    headers = {
        'Content-Type': 'application/x-www-form-urlencoded'
    }
    
    response = requests.post(url, data=data, headers=headers)
    
    if response.status_code == 200:
        print("Token revoked successfully")
        return True
    else:
        print(f"Failed to revoke token: {response.status_code}")
        print(response.text)
        return False

def refresh_twitch_token(client_id, client_secret, refresh_token):
    """
    Refresh a Twitch OAuth token
    """
    url = "https://id.twitch.tv/oauth2/token"
    
    data = {
        'grant_type': 'refresh_token',
        'refresh_token': refresh_token,
        'client_id': client_id,
        'client_secret': client_secret
    }
    
    headers = {
        'Content-Type': 'application/x-www-form-urlencoded'
    }
    
    response = requests.post(url, data=data, headers=headers)
    
    if response.status_code == 200:
        token_data = response.json()
        print("Token refreshed successfully")
        return token_data
    else:
        print(f"Failed to refresh token: {response.status_code}")
        print(response.text)
        return None

# Usage examples:
if __name__ == "__main__":
    # Your app credentials
    CLIENT_ID = ''
    CLIENT_SECRET = ''
    
    # To revoke a token
    access_token = ''
    revoke_twitch_token(CLIENT_ID, access_token)
    
    # To refresh a token
    refresh_token = ''
    new_tokens = refresh_twitch_token(CLIENT_ID, CLIENT_SECRET, refresh_token)
    
    if new_tokens:
        print(f"New access token: {new_tokens['access_token']}")
        print(f"New refresh token: {new_tokens.get('refresh_token', 'None')}")
