# 🗺️ InteractiveNJ
## NJ Transit & GLAM Accessibility Dashboard

This **Shiny dashboard** explores **public transit stops** and **cultural institutions (GLAMs)** across New Jersey.  
It features an **interactive map**, **searchable data tables**, and **demographic visualizations** using U.S. Census ACS data.

---

## 🚀 Getting Started

### 1. Census API Key Setup

To regenerate ACS-based data (e.g., population, income, demographics), you’ll need a **Census API key**.

1. Request a key here:  
   👉 [https://api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html)

2. In R, run:

   ```r
   tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)
