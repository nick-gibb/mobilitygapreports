from pathlib import Path
import subprocess
from mdutils.mdutils import MdUtils
from mdutils import Html
import subprocess
from pathlib import Path
from datetime import datetime
import shutil


def execute_R_scripts():
    """ Call Kevin's R scripts. """
    today = datetime.today().strftime('%Y-%m-%d')
    shutil.rmtree(f'output/{today}')  # for testing
    output = subprocess.call(['sh', './callR.sh'])
    print(output)


def create_markdown():
    """ Create markdown file which Kevin's figures are embedded. """
    today = datetime.today().strftime('%Y-%m-%d')
    mdFile = MdUtils(file_name=today)
    mdFile.new_header(
        level=1, title='Estimates of mobility and the required mobility to control COVID-19 in Canada')
    mdFile.new_paragraph('**Figure 1.** The non-residential mobility index is a measure of the average amount of time spent outside of home, based on smartphone mobility data (the index is scaled so that levels in the baseline period from Jan 3 to Feb 6, 2020 represent 100).')

    mdFile.new_paragraph(Html.image(
        "mobilityAlone_1yr.png", size='900', align='center'))
    mdFile.new_paragraph(
        '**Figure 2.** Association between non-residential mobility and COVID-19 case growth across 7 Canadian provinces, March 15, 2020 to January 16, 2021.')
    mdFile.new_paragraph(Html.image(
        "mobility_byMonth.png", size='900', align='center'))
    mdFile.new_paragraph('Legend: Weekly COVID-19 growth rate (Y = cases in given week / cases in prior week) is strongly associated with the non-residential mobility in the prior 2-week period (X). The point where the regression line (black) crosses the line representing no COVID-19 case growth (red line) is the average Canadian mobility threshold. The Canadian mobility threshold is lower in spring, fall and winter of 2020, compared to the summer of 2020.')
    mdFile.new_paragraph('**Figure 3.** Left panel: Variation in mobility (circles) and the estimated mobility threshold (purple dash), for 5 Canadian provinces with the most cases, March 15, 2020 to January 16, 2021. Right panel: Association between mobility gap and COVID-19 growth rate.')
    mdFile.new_paragraph(Html.image(
        "mobilityGap_both.png", size='900', align='center'))

    mdFile.new_paragraph('Legend: The mobility threshold is the estimated level of mobility needed to control COVID-19 case growth. This threshold is highest in summer and in less populated provinces. When mobility decreased below the mobility threshold (blue dots), weekly COVID-19 cases counts decreased. In November 2020, Manitoba was the only province that implemented a lockdown that had successfully crossed the mobility threshold and has led to reductions in COVID-19 case growth. Other provinces waited until December 2020.')
    mdFile.create_md_file()
    Path(f'{today}.md').rename(f'./output/{today}/Canada/report.md')

    # mdFile.new_line(mdFile.new_inline_image(text='Figure 1', path='images/fig1.png'))
    # mdFile.new_line(mdFile.new_inline_image(text='Figure 2', path='images/fig2.png'))
    # mdFile.new_line(mdFile.new_inline_image(text='Figure 3', path='images/fig3.png'))


def push_to_github():
    """ Push markdown file to Github, triggering a fresh build of mobilitygap.ca """
    pass  # will implement later


def main():
    execute_R_scripts()  # Call Kevin's R scripts to generate the figures
    # Python function to generate the markdown where Kevin's figures will be embedded
    create_markdown()
    push_to_github()


if __name__ == '__main__':
    main()
