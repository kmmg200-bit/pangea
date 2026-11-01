import { useId } from 'react'

/** Серебряная монета. Эмодзи 🪙 на многих телефонах нарисована золотой —
  * рядом с дублонами 🟡 их не отличить, поэтому монета своя. */
export function Silver() {
  const id = useId()
  return (
    <svg className="coin" viewBox="0 0 20 20" role="img" aria-label="серебро">
      <defs>
        <linearGradient id={id} x1="0.15" y1="0.1" x2="0.85" y2="0.95">
          <stop offset="0" stopColor="#fbfcfd" />
          <stop offset="0.45" stopColor="#c9cfd6" />
          <stop offset="1" stopColor="#8b939d" />
        </linearGradient>
      </defs>
      <circle cx="10" cy="10" r="9.2" fill={`url(#${id})`} stroke="#6c747e" strokeWidth="0.9" />
      <circle cx="10" cy="10" r="6.4" fill="none" stroke="#7b838d" strokeWidth="0.8" opacity="0.75" />
      <path d="M10 6.2l1.1 2.4 2.6.3-1.9 1.8.5 2.6L10 12l-2.3 1.3.5-2.6-1.9-1.8 2.6-.3z" fill="#7b838d" opacity="0.85" />
      <path d="M5 6.3a6 6 0 0 1 3.2-2.6" fill="none" stroke="#fff" strokeWidth="1.1" strokeLinecap="round" opacity="0.8" />
    </svg>
  )
}
