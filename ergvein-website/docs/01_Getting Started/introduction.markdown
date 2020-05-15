---
name: Introduction
---
<h1>Introduction</h1>
<p class="lead">A brief guide to get started with Focus theme.</p>

<h2>Files structure</h2>

<p>Unzip the theme folder and you'll see the next file structure:</p>
<ul class="mb-4">
    <li><code>dist</code> - production files</li>
    <li><code>src</code>
        <ul>
            <li><code>assets</code></li>
            <li><code>pages</code></li>
            <li><code>templates</code></li>
        </ul>
    </li>
    <li><code>gulpfile.js</code></li>
    <li><code>package.json</code></li>
    <li><code>README.md</code></li>
</ul>

<h2>Development setup</h2>

<p>Theme's dev tools require Node and Gulp CLI. To install Node go to <a href="https://nodejs.org/en/" target="_blank" rel="noopener">Node.js </a> and follow the instructions. To install <a href="https://gulpjs.com/" target="_blank" rel="noopener">Gulp CLI</a> run the following command:</p>
<pre class="mb-4"><code class="language-bash py-2">npm i gulp-cli -g</code></pre>
<p>Then run the following npm command to install all the theme's dependencies:</p>
<pre class="mb-4"><code class="language-bash py-2">npm i</code></pre>
<p>Running <strong>gulp</strong> will compile the theme, copy all required files to the dist directory and will open a browser window to <code>dist/index.html</code>.</p>
<pre class="mb-4"><code class="language-bash py-2">gulp</code></pre>

<p>Other gulp tasks available:</p>
<ul class="mb-4">
    <li class="mb-2">
        <code>gulp css</code>: Compiles and minifies <code>scss</code> files to <code>dist</code>.
    </li>
    <li class="mb-2">
        <code>gulp watch</code>: Starts a local server and watch for changes.
    </li>
</ul>
<p>The theme uses <a href="https://mozilla.github.io/nunjucks/" target="_blank" rel="noopener">Nunjucks</a> as a template engine, basically to include partials (header, footer...). Gulp renders Nunjucks templates to HTML.</p>

<p><strong>Note:</strong> If you prefer, you can simply use the production files that are in the <strong>dist</strong> folder and bypass the gulp workflow.</p>

<h3>Customization</h3>

<p>There are two ways to customize your theme:</p>
<ul>
    <li class="mb-2"><strong>SCSS</strong>. If you use the gulp workflow to compile the scss files (recommended) then you can use the <code>_user-vars.scss</code> file to add your own variables and <code>_user.css</code> to add your own styles.</li>
    <li class="mb-2"><strong>CSS</strong>. If you use the compiled files, you can simply add a <code>custom.css</code> file in the <code>assets/css</code> folder with your own styles and link it to the head of your pages.</li>
</ul>

<h3>Remove unused CSS</h3>

<p>Once your files are ready for production, you can remove all unused CSS using <a href="https://www.purgecss.com/" target="_blank" rel="noopener">purgecss</a>. To do it just run the following command:</p>
<figure class="mb-4">
    <pre class="mb-4"><code class="language-bash py-2">gulp uncss</code></pre>
</figure>
<p>Purgecss analyzes the HTML and Bootstrap JS files and removes unused CSS. The CSS file size will be reduced and your website performance will improve.</p>
