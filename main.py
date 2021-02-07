from pathlib import Path
import subprocess
from mdutils.mdutils import MdUtils
from mdutils import Html
import subprocess
from pathlib import Path
from datetime import datetime
import shutil
import frontmatter
import io
import glob
from PIL import Image


today = datetime.today().strftime('%Y-%m-%d')
today_text = datetime.today().strftime('%B %-d, %Y')


regionNames = {
    "Canada": "ca",
    "Alberta": "ab",
    "Manitoba": "mb",
    "British Columbia": "bc",
    "Saskatchewan": "sk",
    "Quebec": "qc",
    "Ontario": "on",
    "Newfoundland": "nl",
    "New Brunswick": "nb",
    "Nova Scotia": "ns"
}


def get_img_size(imgpath):
    im = Image.open(f'web_output/public{imgpath}')
    width, height = im.size
    return width, height


def execute_R_scripts():
    """ Call Kevin's R scripts. """
    # shutil.rmtree(f'output/2021-02-04')  # for testing
    output = subprocess.call(['sh', './callR.sh'])
    print(output)


def create_markdown(region):
    """ Create markdown file which Kevin's figures are embedded. """
    region_short = regionNames[region]
    mdFile = MdUtils(file_name=today)
    mdFile.new_line('---')
    mdFile.new_line(f'title: \"{today}\"')
    mdFile.new_line(f'date: "{today}"')
    mdFile.new_line('description: "Some amazing findings in here!"')
    mdFile.new_line(f'region: "{region}"')
    mdFile.write('\n---\n')
    mdFile.new_paragraph('**Figure 1.** The non-residential mobility index is a measure of the average amount of time spent outside of home, based on smartphone mobility data (the index is scaled so that levels in the baseline period from Jan 3 to Feb 6, 2020 represent 100).')
    # <img src="mobilityAlone_1yr.png" style="width:2400px;height:1200px"/>
    img_path = f"/figures/{region_short}/{today}/mobilityAlone_1yr.png"
    width1, height1 = get_img_size(img_path)
    mdFile.new_paragraph(Html.image(
        path=img_path, size=f'{width1}x{height1}', align='center'))
    mdFile.new_paragraph(
        f'**Figure 2.** Association between non-residential mobility and COVID-19 case growth across 7 Canadian provinces, March 15, 2020 to {today_text}.')

    img_2_path = f"/figures/{region_short}/{today}/mobility_byMonth.png"
    width2, height2 = get_img_size(img_2_path)
    mdFile.new_paragraph(Html.image(
        path=img_2_path, size=f'{width2}x{height2}', align='center'))

    mdFile.new_paragraph('Weekly COVID-19 growth rate (Y = cases in given week / cases in prior week) is strongly associated with the non-residential mobility in the prior 2-week period (X). The point where the regression line (black) crosses the line representing no COVID-19 case growth (red line) is the average Canadian mobility threshold. The Canadian mobility threshold is lower in spring, fall and winter of 2020, compared to the summer of 2020.', align="center")
    mdFile.new_paragraph(
        f'**Figure 3.** Left panel: Variation in mobility (circles) and the estimated mobility threshold (purple dash), for 5 Canadian provinces with the most cases, March 15, 2020 to {today_text}, 2021. Right panel: Association between mobility gap and COVID-19 growth rate.')
    img_3_path = f"/figures/{region_short}/{today}/mobilityGap_both.png"
    width3, height3 = get_img_size(img_3_path)
    mdFile.new_paragraph(Html.image(
        path=img_3_path, size=f'{width3}x{height3}', align='center'))

    mdFile.new_paragraph('The mobility threshold is the estimated level of mobility needed to control COVID-19 case growth. This threshold is highest in summer and in less populated provinces. When mobility decreased below the mobility threshold (blue dots), weekly COVID-19 cases counts decreased. In November 2020, Manitoba was the only province that implemented a lockdown that had successfully crossed the mobility threshold and has led to reductions in COVID-19 case growth. Other provinces waited until December 2020.', align="center")
    mdFile.create_md_file()
    Path(f'{today}.md').rename(
        f'./web_output/reports/{region_short}/{today}.md')

    with open(f'./web_output/reports/{region_short}/{today}.md', "r") as f:
        lines = f.readlines()
    with open(f'./web_output/reports/{region_short}/{today}.md', "w") as f:
        for line in lines[4:]:
            f.write(line)


def push_to_github():
    """ Push markdown file to Github, triggering a fresh build of mobilitygap.ca """
    pass  # will implement later


def rearrange_folders():
    p = Path(Path.cwd(), "output", today)
    province_output_paths = [x for x in p.iterdir() if x.is_dir()]
    for province_output_path in province_output_paths:
        prov_long = province_output_path.parts[-1]
        prov_short = regionNames[prov_long]
        print(prov_long)
        # for testing
        shutil.rmtree(
            Path(Path.cwd(), 'web_output/public/figures', prov_short))

        trg_path = Path(Path.cwd(), 'web_output/public/figures',
                        prov_short, today)
        trg_path.mkdir(parents=True, exist_ok=True)
        for src_file in province_output_path.glob('*.*'):
            shutil.copy(src_file, trg_path)


def get_regions():
    p = Path(Path.cwd(), "output", today)
    regions = [x.parts[-1] for x in p.iterdir() if x.is_dir()]
    return regions


def main():
    execute_R_scripts()  # Call Kevin's R scripts to generate the figures
    rearrange_folders()
    # Python function to generate the markdown where Kevin's figures will be embedded
    regions = get_regions()
    for region in regions:
        create_markdown(region)
    push_to_github()


if __name__ == '__main__':
    main()
