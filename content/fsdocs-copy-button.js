// Adds a "Copy" button to every code block so readers can easily copy snippets.
function createCopyButton() {
    const button = document.createElement('button')
    button.className = 'copy-code-button'
    button.setAttribute('aria-label', 'Copy code to clipboard')
    button.textContent = 'Copy'
    return button
}

function attachCopyHandler(button, getText) {
    button.addEventListener('click', function () {
        const text = getText()
        if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText(text).then(
                function () {
                    button.textContent = 'Copied!'
                    setTimeout(function () {
                        button.textContent = 'Copy'
                    }, 2000)
                },
                function () {
                    button.textContent = 'Failed'
                    setTimeout(function () {
                        button.textContent = 'Copy'
                    }, 2000)
                }
            )
        } else {
            // Fallback for non-HTTPS environments
            const el = document.createElement('textarea')
            el.value = text
            document.body.appendChild(el)
            el.select()
            document.execCommand('copy')
            document.body.removeChild(el)
            button.textContent = 'Copied!'
            setTimeout(function () {
                button.textContent = 'Copy'
            }, 2000)
        }
    })
}

document.addEventListener('DOMContentLoaded', function () {
    // Snippets emitted by fsdocs: <div class="fsdocs-snippet"> holding an optional
    // line number <pre> and the code <pre>. Copy only the code column.
    document.querySelectorAll('.fsdocs-snippet').forEach(function (snippet) {
        const code = snippet.querySelector('pre:not(.fsdocs-snippet-lines)')
        if (!code) return

        const button = createCopyButton()
        snippet.appendChild(button)

        attachCopyHandler(button, function () {
            return code.innerText
        })
    })

    // Any other pre > code block on the page, for example hand written HTML.
    document.querySelectorAll('pre > code').forEach(function (code) {
        if (code.closest('.fsdocs-snippet')) return
        const pre = code.parentElement
        pre.classList.add('has-copy-button')

        const button = createCopyButton()
        pre.appendChild(button)

        attachCopyHandler(button, function () {
            return code.innerText
        })
    })
})
