from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import time
from selenium.webdriver.support.ui import Select
#from selenium.webdriver.chrome.options import Options

def checkType(expression):
	js_class = driver.execute_script("return arguments[0].className;", expression)
	print(js_class)

#chrome_options = Options()
#chrome_options.add_argument('--headless')
driver = webdriver.Chrome(executable_path = '/snap/bin/chromium.chromedriver') #, options=chrome_options) #'/usr/bin/google-chrome')

driver.get('http://localhost:3000/')

# Login
login_form = driver.find_element(By.ID, 'new_user')
username_field = login_form.find_element(By.ID, 'user_login')
password_field = login_form.find_element(By.ID, 'user_password')
login_button = login_form.find_element(By.CSS_SELECTOR, 'button[type="submit"]')
username_field.send_keys('kkn')
password_field.send_keys('konrad1991')
login_button.click()
time.sleep(2.5)

current_url = driver.current_url
print(current_url)
page_source = driver.page_source
with open('current_page.html', 'w') as file:
    file.write(page_source)

# Choose Research Plan Tab
tab_list = driver.find_element(By.ID, 'tabList')
research_plan_tab = tab_list.find_element(By.ID, 'tabList-tab-4')
research_plan_tab.click()
time.sleep(2.5)

# Choose Research plan number 1
research_plan_div = driver.find_element(By.XPATH, "//div[@class='preview-table'][text()='New Research Plan']")
research_plan_div.click()
time.sleep(2.5)

# Choose attachment tab
attachment_tab = driver.find_element(By.ID, 'screen-detail-tab-tab-attachments')
attachment_tab.click()
time.sleep(2.5)

# Choose TPA for iris dataset
attachment_divs = driver.find_elements(By.XPATH, "//div[@class='col-md-4']")
attachment_text = 'iris.csv'
attachment = ""

for div in attachment_divs:
	if div.text == attachment_text:
		attachment = div.find_element(By.XPATH, "..")
		dropdown = attachment.find_element(By.ID, 'dropdown')
		send = attachment.find_element(By.XPATH, ".//button[contains(text(), 'Send')]")
		select = Select(dropdown)
		select.select_by_value('Biostats')
		send.click()
		time.sleep(5)
		break

# Switch tab with driver
all_handles = driver.window_handles
for handle in all_handles:
    driver.switch_to.window(handle)
    if "Biostats" in driver.title:
    	break

# Work in Biostats
operation = driver.find_element(By.ID, "op")
operation.clear()
operation.send_keys('Sepal.Length + 100')
modify = driver.find_element(By.ID, "mod")
modify.click()
time.sleep(2.5)

visualisation_tab = driver.find_element(By.XPATH, "//ul[@class='sidebar-menu']//a[contains(text(), 'Visualisation')]")
visualisation_tab.click()
time.sleep(10)

driver.quit()

