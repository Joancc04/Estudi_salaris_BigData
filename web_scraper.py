import requests
from bs4 import BeautifulSoup
import pandas as pd

# URL for 2024 data
url = "https://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2024"
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"
}

# Get page content
response = requests.get(url, headers=headers)
soup = BeautifulSoup(response.content, "html.parser")

# Find the first table
table = soup.find("table")
if not table:
    raise Exception("No table found on the page!")

# Extract table headers
headers_row = table.find_all("tr")[0]
headers = [th.text.strip() for th in headers_row.find_all("th")]

# Extract table rows
rows = []
for tr in table.find_all("tr")[1:]:
    tds = tr.find_all("td")
    if tds:
        row = [td.text.strip() for td in tds]
        rows.append(row)

# Create DataFrame and save as CSV
df = pd.DataFrame(rows, columns=headers)
df.to_csv("cost_of_living_by_country_2024.csv", index=False, encoding='utf-8')
print("CSV exported as cost_of_living_by_country_2024.csv")
