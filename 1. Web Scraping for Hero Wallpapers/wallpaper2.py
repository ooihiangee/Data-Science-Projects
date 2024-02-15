import requests
import os
import time

count = 0
# get file list
source = os.getcwd()
file_list = os.listdir(source)

# headers and url
url = 'https://pvp.qq.com/web201605/js/herolist.json'
headers = {'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36'}
response = requests.get(url,headers=headers)
data = response.json()

for hero in data:
    hero_name = hero['cname']
    hero_code = hero['ename']
    try:
        hero_skins = hero['skin_name']
    except Exception as e:
        continue
    print(hero_name,hero_code,hero_skins)
    hero_skin = hero_skins.split('|') # split the hero_skins into an array of different skins

# https://game.gtimg.cn/images/yxzj/img201606/skin/hero-info/508/508-bigskin-1.jpg

    for i in range(1,15):
        hero_url = f'https://game.gtimg.cn/images/yxzj/img201606/skin/hero-info/{hero_code}/{hero_code}-bigskin-{i}.jpg'

        filename = hero_name+str(i)+'.jpg'

        if filename in file_list:
            print(hero_url)
            print('已截取，跳过')
            continue
        else:
            response = requests.get(hero_url)
            if response.status_code == 200:
                print(hero_url)
                print('正在截取')
                with open(filename,'wb') as f:
                    f.write(response.content)
                count += 1
            else:
                break

    print('休息两秒')
    time.sleep(2)
    print('')

print('新增皮肤数量:'+str(count))
