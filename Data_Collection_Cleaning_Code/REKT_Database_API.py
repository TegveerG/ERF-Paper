import requests
import json

# updated limit or num rows to most recent (28/08/2023) data

url = "https://api.s.defiyield.app/scam_database?sortField=fundsLost&sort=desc&sortDirection=desc&limit=3513&page=1"

payload={}
headers = {}

response = requests.request("GET", url, headers=headers, data=payload)

res_list = response.json()['items'] # items key is pertinent

with open('./Data/REKT_Database_Python_API.json', 'w') as json_file:
    json.dump(res_list, json_file)
